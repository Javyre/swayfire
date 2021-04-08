#include "grab.hpp"
#include "core.hpp"

#include <wayfire/nonstd/wlroots-full.hpp>

// IActiveGrab

std::unique_ptr<IActiveGrab> IActiveGrab::try_activate(
    nonstd::observer_ptr<Swayfire> plugin,
    const std::function<std::unique_ptr<IActiveGrab>()> &cons) {

    if (plugin->output->activate_plugin(plugin->grab_interface)) {
        if (plugin->grab_interface->grab()) {
            return cons();
        } else {
            plugin->output->deactivate_plugin(plugin->grab_interface);
        }
    }

    return nullptr;
}

IActiveGrab::~IActiveGrab() {
    plugin->output->deactivate_plugin(plugin->grab_interface);
}

// IActiveButtonDrag

void IActiveButtonDrag::button(uint32_t b, uint32_t state) {
    if (b == deactivate_button && state == WLR_BUTTON_RELEASED) {
        plugin->active_grab = nullptr;
    }
}

// ActiveMove

void ActiveMove::pointer_motion(uint32_t x, uint32_t y) {
    auto geo = original_geo;
    geo.x += (int)x - pointer_start.x;
    geo.y += (int)y - pointer_start.y;
    dragged->set_geometry(geo);
}

std::unique_ptr<IActiveGrab>
ActiveMove::construct(nonstd::observer_ptr<Swayfire> plugin, Node dragged) {

    return try_activate(plugin, [&]() {
        auto ret =
            std::make_unique<ActiveMove>(plugin, plugin->button_move_activate);
        auto p = wf::get_core().get_cursor_position();

        ret->dragged = dragged;
        ret->original_geo = dragged->get_geometry();
        ret->pointer_start = {(int)p.x, (int)p.y};

        return ret;
    });
}

// ActiveResize

constexpr double RESIZE_MARGIN = 0.35;
uint32_t resize_calc_resizing_edges(wf::geometry_t geo, wf::point_t p) {
    if (!(geo & p)) {
        LOGE("Point not in geometry. Cannot calculate resizing egdes.");
        return WLR_EDGE_NONE;
    }

    uint32_t edges = WLR_EDGE_NONE;

    auto vert_margin = (int)((double)geo.height * RESIZE_MARGIN);
    auto hori_margin = (int)((double)geo.width * RESIZE_MARGIN);

    if ((p.x - geo.x) < hori_margin)
        edges |= WLR_EDGE_LEFT;
    else if ((geo.x + geo.width - p.x) < hori_margin)
        edges |= WLR_EDGE_RIGHT;

    if ((p.y - geo.y) < vert_margin)
        edges |= WLR_EDGE_TOP;
    else if ((geo.y + geo.height - p.y) < vert_margin)
        edges |= WLR_EDGE_BOTTOM;

    if (edges == WLR_EDGE_NONE) {
        if ((p.x - geo.x) < (geo.width / 2))
            edges |= WLR_EDGE_LEFT;
        else
            edges |= WLR_EDGE_RIGHT;

        if ((p.y - geo.y) < (geo.height / 2))
            edges |= WLR_EDGE_TOP;
        else
            edges |= WLR_EDGE_BOTTOM;
    }

    return edges;
}
#undef RESIZE_MARGIN

void ActiveResize::pointer_motion(uint32_t x, uint32_t y) {
    const int dw = (int)x - pointer_start.x;
    const int dh = (int)y - pointer_start.y;

    if (dw == 0 && dh == 0)
        return;

    const int nw = (resizing_edges & WLR_EDGE_LEFT)    ? original_geo.width - dw
                   : (resizing_edges & WLR_EDGE_RIGHT) ? original_geo.width + dw
                                                       : original_geo.width;
    const int nh = (resizing_edges & WLR_EDGE_TOP) ? original_geo.height - dh
                   : (resizing_edges & WLR_EDGE_BOTTOM)
                       ? original_geo.height + dh
                       : original_geo.height;

    // Only actually apply view geometries once the whole tree's geometries have
    // been updates.
    root_node->for_each_leaf([](auto n) { n->push_safe_set_geo(); });
    dragged->try_resize({nw, nh}, resizing_edges);
    root_node->for_each_leaf([](auto n) { n->pop_safe_set_geo(); });
    root_node->refresh_geometry();
}

std::unique_ptr<IActiveGrab>
ActiveResize::construct(nonstd::observer_ptr<Swayfire> plugin, Node dragged) {
    return try_activate(plugin, [&]() {
        auto ret = std::make_unique<ActiveResize>(
            plugin, plugin->button_resize_activate);
        auto p = wf::get_core().get_cursor_position();

        ret->dragged = dragged;
        ret->original_geo = dragged->get_geometry();
        ret->pointer_start = {(int)p.x, (int)p.y};

        ret->resizing_edges =
            resize_calc_resizing_edges(ret->original_geo, ret->pointer_start);

        ret->root_node = ret->dragged->find_floating_parent();
        if (!ret->root_node)
            ret->root_node = ret->dragged->get_ws()->tiled_root.get();

        ret->root_node->begin_resize();

        wf::get_core().set_cursor(
            wlr_xcursor_get_resize_name((wlr_edges)(ret->resizing_edges)));

        return ret;
    });
}

ActiveResize::~ActiveResize() { root_node->end_resize(); }

// Swayfire

void Swayfire::init_grab_interface() {
    grab_interface->name = "swayfire";
    grab_interface->capabilities =
        wf::CAPABILITY_GRAB_INPUT | wf::CAPABILITY_MANAGE_DESKTOP;

    grab_interface->callbacks.pointer.motion = [&](uint32_t x, uint32_t y) {
        if (active_grab)
            active_grab->pointer_motion(x, y);
    };

    grab_interface->callbacks.pointer.button = [&](uint32_t b, uint32_t state) {
        if (active_grab)
            active_grab->button(b, state);
    };

    grab_interface->callbacks.touch.motion = [&](int32_t id, int32_t x,
                                                 int32_t y) {
        if (active_grab && id == 1)
            active_grab->pointer_motion(x, y);
    };

    on_move_activate = [&](auto) {
        if (auto view = wf::get_core().get_cursor_focus_view()) {
            if (auto vdata = view->get_data<ViewData>()) {
                if (auto node = vdata->node->find_floating_parent()) {
                    if (auto active = ActiveMove::construct(this, node)) {
                        active_grab = std::move(active);
                        return true;
                    }
                }
            }
        }
        return false;
    };

    on_resize_activate = [&](auto) {
        if (auto view = wf::get_core().get_cursor_focus_view()) {
            if (auto vdata = view->get_data<ViewData>()) {
                if (auto active = ActiveResize::construct(this, vdata->node)) {
                    active_grab = std::move(active);
                    return true;
                }
            }
        }
        return false;
    };

    output->add_button(button_move_activate, &on_move_activate);
    output->add_button(button_resize_activate, &on_resize_activate);
}

void Swayfire::fini_grab_interface() {
    output->rem_binding(&on_resize_activate);
    output->rem_binding(&on_move_activate);

    active_grab = nullptr;
}
