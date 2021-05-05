#include "deco.hpp"

// Decoration

// TODO: damage only the region of the deco and not the whole bounding box.
void ViewDecoration::damage() {
    node->view->damage();
}

wf::geometry_t ViewDecoration::expand_wm_geometry(wf::geometry_t content) {
    // TODO: implement window titles in deco.
    content.x -= options->border_width;
    content.y -= options->border_width;
    content.width += 2 * options->border_width;
    content.height += 2 * options->border_width;
    return content;
}

void ViewDecoration::calculate_resize_size(int &target_width,
                                           int &target_height) {
    target_width -= 2 * options->border_width;
    target_height -= 2 * options->border_width;

    target_width = std::max(target_width, 1);
    target_height = std::max(target_height, 1);
}

void ViewDecoration::notify_view_activated(bool active) {
    colors = active ? &options->colors.focused : &options->colors.unfocused;
    damage();
}

void ViewDecoration::notify_view_resized(wf::geometry_t view_geometry) {
    node->view->damage();
    width = view_geometry.width;
    height = view_geometry.height;
    node->view->damage();
}

bool ViewDecoration::is_mapped() const { return true; }

wf::point_t ViewDecoration::get_offset() {
    return {-options->border_width, -options->border_width};
}

wf::dimensions_t ViewDecoration::get_size() const { return {width, height}; }

void ViewDecoration::simple_render(const wf::framebuffer_t &fb, int x, int y,
                                   const wf::region_t &damage) {
    auto split_type = node->get_prefered_split_type();

    wf::color_t border_color = colors->child_border;
    wf::color_t indi_color = colors->indicator;
    int radius = options->border_radius;
    int bw = options->border_width;

    OpenGL::render_begin(fb);
    for (auto scissor : damage) {
        fb.logic_scissor(wlr_box_from_pixman_box(scissor));

        // left side
        OpenGL::render_rectangle(
            {
                x,
                y + radius,
                bw,
                height - (2 * radius),
            },
            border_color, fb.get_orthographic_projection());

        // right side
        OpenGL::render_rectangle(
            {
                x + width - bw,
                y + radius,
                bw,
                height - (2 * radius),
            },
            split_type == SplitType::VSPLIT ? indi_color : border_color,
            fb.get_orthographic_projection());

        // top side
        OpenGL::render_rectangle(
            {
                x + radius,
                y,
                width - (2 * radius),
                bw,
            },
            border_color, fb.get_orthographic_projection());

        // bottom side
        OpenGL::render_rectangle(
            {
                x + radius,
                y + height - bw,
                width - (2 * radius),
                bw,
            },
            split_type == SplitType::HSPLIT ? indi_color : border_color,
            fb.get_orthographic_projection());

        // render corners
    }
    OpenGL::render_end();
}

// SwayfireDeco

void SwayfireDeco::decorate_node(Node node) {
    if (auto vnode = node->as_view_node()) {
        LOGD("Decorating ", vnode);
        auto surf = std::make_unique<ViewDecoration>(vnode, &options);
        vnode->view->set_decoration(surf.get());
        vnode->view->add_subsurface(std::move(surf), false);
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

    swayfire->workspaces.for_each([&](WorkspaceRef ws) {
        ws->for_each_node([&](Node n) { decorate_node(n); });
    });

    output->connect_signal("swf-view-node-attached", &on_view_node_attached);
}

void SwayfireDeco::swf_fini() { LOGD("=== deco fini ==="); }

DECLARE_WAYFIRE_PLUGIN(SwayfireDeco)
