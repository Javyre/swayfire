#include "deco.hpp"

// Decoration

wf::region_t ViewDecoration::calculate_region() {
    wf::region_t region;

    for (const auto &ss : subsurfs)
        region |= ss->calculate_region();

    return region;
}

// TODO: damage only the region of the deco and not the whole bounding box.
void ViewDecoration::damage() { node->view->damage(); }

wf::geometry_t ViewDecoration::expand_wm_geometry(wf::geometry_t content) {
    // TODO: implement window titles in deco.
    if (is_hidden())
        return content;

    content.x -= options->border_width;
    content.y -= options->border_width;
    content.width += 2 * options->border_width;
    content.height += 2 * options->border_width;
    return content;
}

void ViewDecoration::calculate_resize_size(int &target_width,
                                           int &target_height) {
    if (is_hidden())
        return;

    target_width -= 2 * options->border_width;
    target_height -= 2 * options->border_width;

    target_width = std::max(target_width, 1);
    target_height = std::max(target_height, 1);
}

void ViewDecoration::notify_view_activated(bool active) {
    colors = active ? &options->colors.focused : &options->colors.unfocused;
    damage();
}

// NOTE: This also gets called when first decorating the view, so this is when
// we first properly get notified of its geometry and initialize cached_region.
void ViewDecoration::notify_view_resized(wf::geometry_t view_geometry) {
    node->view->damage();
    width = view_geometry.width;
    height = view_geometry.height;
    cached_region = calculate_region();
    node->view->damage();
}

void ViewDecoration::hide(bool enable) {
    hidden = enable;
    // TODO: remove this code when
    // https://github.com/WayfireWM/wayfire/pull/1187 gets released.
    // It should be node->view->damage().
    node->refresh_geometry();
}

bool ViewDecoration::is_hidden() const {
    return hidden || node->view->fullscreen;
}

bool ViewDecoration::is_mapped() const { return mapped; }

wf::point_t ViewDecoration::get_offset() {
    if (is_hidden())
        return {0, 0};
    return {-options->border_width, -options->border_width};
}

wf::dimensions_t ViewDecoration::get_size() const { return {width, height}; }

bool ViewDecoration::accepts_input(int32_t sx, int32_t sy) {
    for (const auto &ss : subsurfs)
        if (ss->contains_point({sx, sy}))
            return true;
    return false;
}

void ViewDecoration::simple_render(const wf::framebuffer_t &fb, int x, int y,
                                   const wf::region_t &damage) {
    if (is_hidden())
        return;

    const wf::region_t region = cached_region + wf::point_t{x, y};

    OpenGL::render_begin(fb);
    for (const auto &scissor : region &damage) {
        fb.logic_scissor(wlr_box_from_pixman_box(scissor));

        for (const auto &ss : subsurfs)
            ss->render({x, y}, fb.get_orthographic_projection());
    }

    OpenGL::render_end();
}

// SwayfireDeco

void SwayfireDeco::decorate_node(Node node) {
    if (auto vnode = node->as_view_node()) {
        LOGD("Decorating ", vnode);
        auto surf = std::make_unique<ViewDecoration>(vnode, &options);
        auto surfref = surf.get();
        vnode->view->add_subsurface(std::move(surf), false);
        vnode->view->set_decoration(surfref);
        vnode->refresh_geometry();
    } else if (auto snode = node->as_split_node()) {
        // TODO: implement split node decorations.
        (void)snode;
    }
}

void SwayfireDeco::swf_init() {
    LOGD("=== deco init ===");
    grab_interface->name = "swayfire-deco";
    grab_interface->capabilities = wf::CAPABILITY_VIEW_DECORATOR;

    if (!output->activate_plugin(grab_interface))
        LOGE("Swayfire-deco capability grab failed.");

    subsurf_gl_init();

    swayfire->workspaces.for_each([&](WorkspaceRef ws) {
        ws->for_each_node([&](Node n) { decorate_node(n); });
    });

    output->connect_signal("swf-view-node-attached", &on_view_node_attached);

    options.set_callback(
        [&] { output->emit_signal("swf-deco-config-changed", nullptr); });
}

void SwayfireDeco::swf_fini() {
    LOGD("=== deco fini ===");
    output->emit_signal("swf-deco-fini", nullptr);
    output->disconnect_signal(&on_view_node_attached);

    subsurf_gl_fini();
}

DECLARE_WAYFIRE_PLUGIN(SwayfireDeco)
