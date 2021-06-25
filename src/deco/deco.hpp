#ifndef SWAYFIRE_DECO_HPP
#define SWAYFIRE_DECO_HPP

#include <utility>
#include <wayfire/compositor-surface.hpp>
#include <wayfire/decorator.hpp>
#include <wayfire/option-wrapper.hpp>
#include <wayfire/plugin.hpp>
#include <wayfire/surface.hpp>

#include "../core/core.hpp"
#include "../core/plugin.hpp"
#include "subsurf.hpp"

struct DecorationColors {
    wf::option_wrapper_t<wf::color_t> border, background, text, indicator,
        child_border;

    /// Set a callback to execute when the option values change.
    void set_callback(const std::function<void()> &cb) {
        border.set_callback(cb);
        background.set_callback(cb);
        text.set_callback(cb);
        indicator.set_callback(cb);
    }
};

struct Options {
    wf::option_wrapper_t<int> border_width{"swayfire-deco/border_width"};
    wf::option_wrapper_t<int> border_radius{"swayfire-deco/border_radius"};
    wf::option_wrapper_t<bool> title_bar{"swayfire-deco/title_bar"};

    struct DecoColorSets {
        /// Focused deco color set.
        DecorationColors focused{
            {"swayfire-deco/focused.border"},
            {"swayfire-deco/focused.background"},
            {"swayfire-deco/focused.text"},
            {"swayfire-deco/focused.indicator"},
            {"swayfire-deco/focused.child_border"},
        };
        /// Focused-inactive deco color set.
        DecorationColors focused_inactive{
            {"swayfire-deco/focused_inactive.border"},
            {"swayfire-deco/focused_inactive.background"},
            {"swayfire-deco/focused_inactive.text"},
            {"swayfire-deco/focused_inactive.indicator"},
            {"swayfire-deco/focused_inactive.child_border"},
        };
        /// Unfocused deco color set.
        DecorationColors unfocused{
            {"swayfire-deco/unfocused.border"},
            {"swayfire-deco/unfocused.background"},
            {"swayfire-deco/unfocused.text"},
            {"swayfire-deco/unfocused.indicator"},
            {"swayfire-deco/unfocused.child_border"},
        };

        // TODO: implement other i3 class colors
    } colors;

    /// Set a callback to execute when the option values change.
    void set_callback(const std::function<void()> &cb) {
        border_width.set_callback(cb);
        border_radius.set_callback(cb);
        title_bar.set_callback(cb);

        colors.focused.set_callback(cb);
        colors.focused_inactive.set_callback(cb);
        colors.unfocused.set_callback(cb);
    }
};

class DecorationSurface : public wf::compositor_surface_t,
                          public wf::surface_interface_t {
  private:
    const ViewNodeRef node; ///< The node we're decorating.

    /// Whether the surface is mapped or not.
    bool mapped = true;

    /// The loaded options from the cfg.
    nonstd::observer_ptr<Options> options;

    /// The current color set.
    nonstd::observer_ptr<DecorationColors> colors =
        &(options->colors.unfocused);

    [[nodiscard]] BorderSubSurf::Spec get_border_spec() const;

    wf::dimensions_t size; ///< Size of the decoration.

    wf::region_t cached_region; ///< Cached minimal region containing this deco.

  public:
    DecorationSurface(ViewNodeRef node, nonstd::observer_ptr<Options> options)
        : node(node), options(options) {}

    // Set the size of the surface.
    void set_size(wf::dimensions_t view_size);

    // Set the surface color as active or inactive.
    void set_active(bool active);

    // Recalculate the region and cache it.
    void recalculate_region();

    // Unmap the surface.
    void unmap();

    // == Impl wf::surface_interface_t ==
    bool is_mapped() const override;
    wf::point_t get_offset() override;
    wf::dimensions_t get_size() const override;
    bool accepts_input(int32_t sx, int32_t sy) override;
    void simple_render(const wf::framebuffer_t &fb, int x, int y,
                       const wf::region_t &damage) override;
};

class ViewDecoration : public wf::decorator_frame_t_t {
  private:
    const ViewNodeRef node; ///< The node we're decorating.

    /// Surface representing the decoration.
    nonstd::observer_ptr<DecorationSurface> surface_ref;

    /// Surface swap, used when hiding the surface from the node.
    std::unique_ptr<wf::surface_interface_t> surface;

    /// The loaded options from the cfg.
    nonstd::observer_ptr<Options> options;

    wf::signal_connection_t on_prefered_split_type_changed =
        [&](wf::signal_data_t *) { damage(); };

    wf::signal_connection_t on_config_changed = [&](wf::signal_data_t *) {
        // Refresh geometry in case border_width changes.
        node->refresh_geometry();
        surface_ref->recalculate_region();
        node->view->damage();
    };

    wf::signal_connection_t on_detached = [&](wf::signal_data_t *) {
        surface_ref->unmap();
        if (!is_hidden())
            detach_surface();

        // Save the current node in case cleaning the data triggers a
        // destruction of the current decoration. Avoid crashing when trying to
        // access to the node.
        auto vnode = node;
        vnode->view->set_decoration(nullptr); // ViewDecoration dies here.
    };

    wf::signal_connection_t on_fullscreen = [&](wf::signal_data_t *) {
        if (node->view->fullscreen) {
            if (!is_hidden())
                detach_surface();
        } else {
            if (is_hidden())
                attach_surface();
        }
        node->view->damage();
    };

    // Attach the decoration surface to the node;
    void attach_surface();

    // Detach the decoration surface from the node;
    void detach_surface();

  public:
    ViewDecoration(ViewNodeRef node, nonstd::observer_ptr<Options> options)
        : node(node), options(options) {

        node->connect_signal("prefered-split-type-changed",
                             &on_prefered_split_type_changed);
        node->connect_signal("detached", &on_detached);
        node->view->connect_signal("fullscreen", &on_fullscreen);

        const auto output = node->get_ws()->output;
        output->connect_signal("swf-deco-fini", &on_detached);
        output->connect_signal("swf-deco-config-changed", &on_config_changed);

        auto surface_unique_ptr =
            std::make_unique<DecorationSurface>(node, options);
        surface_ref = surface_unique_ptr.get();
        surface = std::move(surface_unique_ptr);
        if (!node->view->fullscreen)
            attach_surface();
    }

    ~ViewDecoration() override {
        if (!is_hidden())
            detach_surface();

        const auto output = node->get_ws()->output;
        output->disconnect_signal(&on_config_changed);
        output->disconnect_signal(&on_detached);

        node->view->disconnect_signal(&on_fullscreen);
        node->disconnect_signal(&on_detached);
        node->disconnect_signal(&on_prefered_split_type_changed);
    }

    /// Is the decoration currently hidden.
    bool is_hidden() const;

    /// Damage the decoration region.
    void damage();

    // == Impl wf::decorator_frame_t_t ==
    wf::geometry_t expand_wm_geometry(wf::geometry_t content) override;
    void calculate_resize_size(int &target_width, int &target_height) override;
    void notify_view_activated(bool active) override;
    void notify_view_resized(wf::geometry_t view_geometry) override;
    /* TODO: impl these handlers
    void notify_view_tiled() override;
    */
};

class SwayfireDeco : public SwayfirePlugin {
  private:
    /// Add decorations to the node.
    void decorate_node(Node node);

    wf::signal_connection_t on_view_node_attached =
        [&](wf::signal_data_t *data) {
            auto vnode = get_signaled_view_node(data);
            decorate_node(vnode);
        };

    Options options{};

  public:
    // == Impl SwayfirePlugin ==
    void swf_init() override;
    void swf_fini() override;
};

#endif // ifndef SWAYFIRE_DECO_HPP
