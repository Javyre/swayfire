#include "deco.hpp"

// Decoration

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
    surface_ref->set_active(active);
    if (!is_hidden())
        damage();
}

// NOTE: This also gets called when first decorating the view, so this is when
// we first properly get notified of its geometry and initialize the surface
// region.
void ViewDecoration::notify_view_resized(wf::geometry_t view_geometry) {
    node->view->damage();
    surface_ref->set_size(wf::dimensions(view_geometry));
    if (!is_hidden())
        node->view->damage();
}

bool ViewDecoration::is_hidden() const { return (bool)surface; }

void ViewDecoration::attach_surface() {
    assert(surface);
    node->view->add_subsurface(std::move(surface), false);
}

void ViewDecoration::detach_surface() {
    assert(!surface);
    surface = node->view->remove_subsurface(surface_ref);
}

// DecorationSurface

BorderSubSurf::Spec DecorationSurface::get_border_spec() const {
    return {
        {0, 0, size.width, size.height},
        options->border_radius,
        options->border_width,
    };
}

void DecorationSurface::set_active(bool node_active) {
    colors =
        node_active ? &options->colors.focused : &options->colors.unfocused;
}

bool DecorationSurface::is_mapped() const { return mapped; }

void DecorationSurface::unmap() {
    mapped = false;
    wf::emit_map_state_change(this);
}

wf::point_t DecorationSurface::get_offset() {
    return {-options->border_width, -options->border_width};
}

wf::dimensions_t DecorationSurface::get_size() const { return size; }

// NOTE: This also gets called when first decorating the view, so this is when
// we first properly get notified of its geometry and initialize cached_region.
void DecorationSurface::set_size(wf::dimensions_t view_size) {
    size = view_size;
    recalculate_region();
}

void DecorationSurface::recalculate_region() {
    cached_region = BorderSubSurf::calculate_region(get_border_spec());
}

bool DecorationSurface::accepts_input(int32_t sx, int32_t sy) {
    return BorderSubSurf::contains_point(get_border_spec(), {sx, sy});
}

void DecorationSurface::simple_render(const wf::framebuffer_t &fb, int x, int y,
                                      const wf::region_t &damage) {
    const wf::region_t region = cached_region + wf::point_t{x, y};
    const auto spec = get_border_spec();
    const auto color_spec = BorderSubSurf::Colors{
        // all
        colors->child_border,

        // right
        node->get_prefered_split_type() == SplitType::VSPLIT
            ? colors->indicator.value()
            : colors->child_border.value(),

        // bottom
        node->get_prefered_split_type() == SplitType::HSPLIT
            ? colors->indicator.value()
            : colors->child_border.value(),
    };

    OpenGL::render_begin(fb);
    for (const auto &scissor : region &damage) {
        fb.logic_scissor(wlr_box_from_pixman_box(scissor));

        BorderSubSurf::render(spec, color_spec, {x, y},
                              fb.get_orthographic_projection());
    }

    OpenGL::render_end();
}

// SplitDecoration

#define WITH_TABS_SPEC_IMPL                                                    \
    const auto count = tab_surfaces.size();                                    \
    int offset = 0;                                                            \
                                                                               \
    if (node->get_split_type() == SplitType::TABBED) {                         \
        for (std::size_t i = 0; i < count; i++) {                              \
            const TitleBarSubSurf::Spec spec = {                               \
                wf::geometry_t{                                                \
                    offset,                                                    \
                    0,                                                         \
                    i == count - 1                                             \
                        ? geometry.width - offset                              \
                        : (int)((std::size_t)geometry.width / count),          \
                    geometry.height,                                           \
                },                                                             \
                {                                                              \
                    i == 0 ? options->border_radius : 0,                       \
                    i == count - 1 ? options->border_radius : 0,               \
                }};                                                            \
                                                                               \
            f(tab_surfaces[i], spec);                                          \
            offset += (int)((std::size_t)geometry.width / count);              \
        }                                                                      \
    } else if (node->get_split_type() == SplitType::STACKED) {                 \
        for (std::size_t i = 0; i < count; i++) {                              \
            const TitleBarSubSurf::Spec spec = {                               \
                wf::geometry_t{                                                \
                    0,                                                         \
                    (int)i * 20,                                               \
                    geometry.width,                                            \
                    20,                                                        \
                },                                                             \
                {                                                              \
                    i == 0 ? options->border_radius : 0,                       \
                    i == 0 ? options->border_radius : 0,                       \
                }};                                                            \
                                                                               \
            f(tab_surfaces[i], spec);                                          \
        }                                                                      \
    }

void SplitDecoration::with_tabs_spec(
    const std::function<void(TitleBarSubSurf &tab, TitleBarSubSurf::Spec)> &f) {
    WITH_TABS_SPEC_IMPL;
}
void SplitDecoration::with_tabs_spec(
    const std::function<void(const TitleBarSubSurf &tab, TitleBarSubSurf::Spec)>
        &f) const {
    WITH_TABS_SPEC_IMPL;
}
#undef WITH_TABS_SPEC_IMPL

void SplitDecoration::cache_textures() {
    assert(node->get_children_count() == tab_surfaces.size());

    OpenGL::render_begin();
    std::size_t i = 0;
    with_tabs_spec([&](TitleBarSubSurf &tab, const auto spec) {
        const auto child = node->child_at(i);
        const std::string title =
            static_cast<IDisplay *>(child.get())->to_string();

        tab.cache_textures({
            spec,
            options->title_font.value(),
            title,
            wf::color_t(1, 1, 1, 1),
        });
        i++;
    });
    OpenGL::render_end();
    damage();
}

void SplitDecoration::set_size(wf::dimensions_t dims) {
    damage();
    geometry = {
        geometry.x,
        geometry.y,
        dims.width,
        dims.height,
    };
    cache_textures();
    cached_region = calculate_region();

    Padding delta_padding{0, 0, dims.height, 0};
    delta_padding -= current_padding;

    if (delta_padding) {
        node->add_padding(delta_padding);
        current_padding += delta_padding;
        node->refresh_geometry();
    }
}

wf::region_t SplitDecoration::calculate_region() const {
    wf::region_t region;

    with_tabs_spec([&](const auto &tab, const auto spec) {
        region |= tab.calculate_region(spec);
    });

    return region;
}

void SplitDecoration::on_child_inserted_impl() {
    tab_surfaces.emplace_back();

    if (node->get_split_type() == SplitType::STACKED)
        refresh_size();
    else
        cache_textures();
}

void SplitDecoration::on_child_removed_impl() {
    if (node_state.is_child_active) {
        on_set_child_active(false);

        // Notify parents of child possibly no longer in their tree.
        // If the child is still in their tree they will be notified by the
        // on_set_active event.
        auto parent = node->parent->as_split_node();
        while (parent) {
            if (auto deco_data = parent->get_data<SplitDecorationData>())
                deco_data->deco->on_set_child_active(false);

            parent = parent->parent->as_split_node();
        }
    }

    tab_surfaces.pop_back();

    if (node->get_split_type() == SplitType::STACKED)
        refresh_size();
    else
        cache_textures();
}

void SplitDecoration::refresh_size() {
    switch (node->get_split_type()) {
    case SplitType::TABBED:
        set_size({geometry.width, 20});
        break;
    case SplitType::STACKED:
        set_size({geometry.width, 20 * (int)node->get_children_count()});
        break;

    default:
        set_size({0, 0});
        break;
    }
}

void SplitDecoration::on_set_active(bool active) {
    node_state.is_active = active;
    damage();
}

void SplitDecoration::on_set_child_active(bool active) {
    node_state.is_child_active = active;
    damage();
}

bool SplitDecoration::is_mapped() const { return mapped; }

wf::dimensions_t SplitDecoration::get_size() const {
    return wf::dimensions(geometry);
}

bool SplitDecoration::accepts_input(int32_t sx, int32_t sy) {
    bool r = false;

    with_tabs_spec([&](const auto &tab, const auto spec) {
        r = r || tab.contains_point(spec, {sx, sy});
    });

    return r;
}

void SplitDecoration::simple_render(const wf::framebuffer_t &fb, int x, int y,
                                    const wf::region_t &damage) {
    if (tab_surfaces.empty())
        return;

    const auto active_node = node->get_ws()->get_active_node();

    const auto &colors = options->colors;
    const wf::color_t unfocused_color_spec = colors.unfocused.child_border;
    const wf::color_t focused_color_spec = colors.focused.child_border;
    const wf::color_t focused_inctive_color_spec =
        colors.focused_inactive.child_border;

    const wf::region_t region = cached_region + wf::point_t{x, y};

    OpenGL::render_begin(fb);
    for (const auto &scissor : region &damage) {
        fb.logic_scissor(wlr_box_from_pixman_box(scissor));

        const auto matrix = fb.get_orthographic_projection();

        std::size_t i = 0;
        with_tabs_spec([&](const auto &tab, const auto spec) {
            const auto child = node->child_at(i);
            auto color = unfocused_color_spec;

            if (node_state.is_active || active_node == child)
                color = focused_color_spec;
            else if (node_state.is_child_active &&
                     node->get_active_child() == child)
                color = focused_inctive_color_spec;
            else
                color = unfocused_color_spec;

            tab.render(spec, color, {x, y}, matrix);
            i++;
        });
    }

    OpenGL::render_end();
}

void SplitDecoration::move(int x, int y) {
    (void)x;
    (void)y;
}

void SplitDecoration::close() {
    set_size({0, 0});
    node->get_ws()->output->workspace->remove_view(this);
    mapped = false;
    wf::emit_map_state_change(this);

    unref();
}

wf::geometry_t SplitDecoration::get_output_geometry() {
    const auto ws = node->get_ws();
    const auto wsid = ws->wsid;
    const auto curr_wsid = ws->output->workspace->get_current_workspace();
    auto geo = geometry;

    if (wsid != curr_wsid)
        geo =
            nonwf::local_to_relative_geometry(geo, wsid, curr_wsid, ws->output);
    return geo;
}

// SwayfireDeco

void SwayfireDeco::decorate_node(Node node) {
    LOGD("Decorating ", node);
    if (auto vnode = node->as_view_node()) {
        auto deco = std::make_unique<ViewDecoration>(vnode, &options);
        vnode->view->set_decoration(std::move(deco));
    } else if (auto snode = node->as_split_node()) {
        auto surf = std::make_unique<SplitDecoration>(snode, &options);
        const auto surf_ref = surf.get();

        surf->set_output(snode->get_ws()->output.get());
        wf::get_core().add_view(std::move(surf)); // Initialize view

        snode->add_subsurface(surf_ref);
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
    output->connect_signal("swf-split-node-attached", &on_split_node_created);
    output->connect_signal("swf-active-node-changed", &on_active_node_changed);

    options.set_callback(
        [&] { output->emit_signal("swf-deco-config-changed", nullptr); });
}

void SwayfireDeco::swf_fini() {
    LOGD("=== deco fini ===");
    output->emit_signal("swf-deco-fini", nullptr);
    output->disconnect_signal(&on_active_node_changed);
    output->disconnect_signal(&on_split_node_created);
    output->disconnect_signal(&on_view_node_attached);

    subsurf_gl_fini();
}

DECLARE_WAYFIRE_PLUGIN(SwayfireDeco)
