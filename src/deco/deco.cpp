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
                                                                               \
    for (std::size_t i = 0; i < count; i++) {                                  \
        const TitleBarSubSurf::Spec spec = {                                   \
            wf::geometry_t{                                                    \
                (int)(i * (std::size_t)geometry.width / count),                \
                0,                                                             \
                (int)(geometry.width / (std::size_t)count),                    \
                geometry.height,                                               \
            },                                                                 \
            {                                                                  \
                0,                                                             \
                0,                                                             \
            }};                                                                \
                                                                               \
        f(tab_surfaces[i], spec);                                              \
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
    LOGD(node, ": split-deco: ", node->get_children_count(),
         " == ", tab_surfaces.size());
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

// TODO: put this method in a base class and share it
wf::region_t SplitDecoration::calculate_region() const {
    /* wf::region_t region; */

    /* with_tabs_spec([&](const auto &tab, const auto spec) { */
    /*     region |= tab.calculate_region(spec); */
    /* }); */

    /* return region; */
    return wf::geometry_t{0, 0, geometry.width, geometry.height};
}

void SplitDecoration::on_geometry_changed() {
    auto inner_geo = node->get_inner_geometry();
    LOGD(node, ": geo changed: ", inner_geo);

    // Titlebars have constant height
    const bool titlebar_size_changed = geometry.width != inner_geo.width;

    geometry = wf::geometry_t{
        inner_geo.x,
        inner_geo.y - geometry.height,
        inner_geo.width,
        geometry.height,
    };

    if (titlebar_size_changed)
        cache_textures();
    cached_region = calculate_region();
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
    /* if (node->empty()) */
    /*     return; */

    const wf::region_t region = cached_region + wf::point_t{x, y};
    wf::color_t color_spec = colors->child_border;

    const wf::geometry_t region_extents =
        wlr_box_from_pixman_box(region.get_extents());
    const wf::geometry_t damage_extents =
        wlr_box_from_pixman_box(damage.get_extents());

    const wf::geometry_t bbox = get_untransformed_bounding_box();
    LOGD(node, ": bbox = ", bbox);
    LOGD(node, ": region = ", region_extents);
    LOGD(node, ": damage = ", damage_extents);

    /* if ((region & damage).empty()) */
    /*     return; */

    OpenGL::render_begin(fb);
    for (const auto &scissor : region) {
        fb.logic_scissor(wlr_box_from_pixman_box(scissor));

        const auto matrix = fb.get_orthographic_projection();

        int i = 0;
        with_tabs_spec([&](const auto &tab, const auto spec) {
            color_spec.g = color_spec.g * 2;
            tab.render(spec, color_spec, {x, y}, matrix);
            i++;
        });
    }
    /* for (const auto &scissor : damage  & region) { */
    /*     fb.logic_scissor(wlr_box_from_pixman_box(scissor)); */
    /*     const auto matrix = fb.get_orthographic_projection(); */
    /*     RectSubSurf::render(damage_extents, {1, 1, 0.1 * node->get_id(), 1},
     * {0,0}, matrix); */
    /* } */

    OpenGL::render_end();
}

void SplitDecoration::move(int x, int y) {
    LOGD("not moving split deco: ", x, ",", y);
}

void SplitDecoration::close() {
    node->get_ws()->output->workspace->remove_view(this);
    mapped = false;
    wf::emit_map_state_change(this);
    // TODO: remove padding here
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
    if (auto vnode = node->as_view_node()) {
        LOGD("Decorating ", vnode);
        auto deco = std::make_unique<ViewDecoration>(vnode, &options);
        vnode->view->set_decoration(std::move(deco));
    } else if (auto snode = node->as_split_node()) {
        // TODO: implement split node decorations.
        auto surf = std::make_unique<SplitDecoration>(snode, &options);
        snode->add_padding({0, 0, 20, 0});
        snode->add_subsurface(std::move(surf));
        /* snode->store_data(std::make_unique<SplitDecoration>(snode,
         * &options)); */
        /* (void)snode; */
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
