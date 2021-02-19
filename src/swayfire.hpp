#ifndef SWAYFIRE_HPP
#define SWAYFIRE_HPP

#include <bits/stdint-intn.h>
#include <bits/stdint-uintn.h>
#include <memory>
#include <sys/types.h>
#include <variant>
#include <vector>

#include <wayfire/config/types.hpp>
#include <wayfire/core.hpp>
#include <wayfire/geometry.hpp>
#include <wayfire/nonstd/noncopyable.hpp>
#include <wayfire/nonstd/observer_ptr.h>
#include <wayfire/object.hpp>
#include <wayfire/option-wrapper.hpp>
#include <wayfire/output.hpp>
#include <wayfire/plugin.hpp>
#include <wayfire/signal-definitions.hpp>
#include <wayfire/util/log.hpp>
#include <wayfire/workspace-manager.hpp>

#define FLOATING_MOVE_STEP 5

using output_ref_t = nonstd::observer_ptr<wf::output_t>;

namespace nonwf {

wf::point_t get_view_workspace(wayfire_view view, output_ref_t output);

// Convert workspace-local geo to relative-to-current-workspace geo
wf::geometry_t local_to_relative_geometry(wf::geometry_t geo, wf::point_t wsid,
                                          output_ref_t output);

wf::point_t geometry_center(wf::geometry_t geo);

} // namespace nonwf

enum struct split_type_t : uint8_t {
    VSPLIT,
    HSPLIT,
    TABBED,
    STACKED,
};

enum struct direction_t : uint8_t {
    UP,
    DOWN,
    LEFT,
    RIGHT,
};

inline direction_t opposite_dir(direction_t dir) {
    switch (dir) {
    case direction_t::LEFT:
        return direction_t::RIGHT;
    case direction_t::RIGHT:
        return direction_t::LEFT;

    case direction_t::DOWN:
        return direction_t::UP;
    case direction_t::UP:
        return direction_t::DOWN;
    }
}

class node_interface_t;
class split_node_t;
class view_node_t;

using owned_node_t = std::unique_ptr<node_interface_t>;
using node_t = nonstd::observer_ptr<node_interface_t>;
using node_iter_t = std::vector<owned_node_t>::iterator;

using split_node_ref_t = nonstd::observer_ptr<split_node_t>;
using view_node_ref_t = nonstd::observer_ptr<view_node_t>;

class display_interface_t {
  public:
    virtual std::string to_string() const {
        std::ostringstream out;
        out << this;
        return out.str();
    };
    virtual std::ostream &to_stream(std::ostream &os) const = 0;

    virtual ~display_interface_t() = default;
};

inline std::ostream &operator<<(std::ostream &os,
                                const display_interface_t &n) {
    return n.to_stream(os);
}

template <class T>
inline std::ostream &operator<<(std::ostream &os,
                                const nonstd::observer_ptr<T> &n) {
    if (n)
        return n->to_stream(os);
    os << "(null)";
    return os;
}

class node_parent_interface_t : public virtual display_interface_t {
  public:
    split_node_ref_t as_split_node();

    virtual node_t get_adjacent(node_t node, direction_t dir) = 0;
    virtual bool move_child(node_t node, direction_t dir) = 0;

    virtual node_t get_last_active_node() = 0;

    virtual void insert_child(owned_node_t node) = 0;
    virtual owned_node_t remove_child(node_t node) = 0;
    virtual owned_node_t swap_child(node_t node, owned_node_t other) = 0;
    virtual void set_active_child(node_t node) = 0;

    virtual ~node_parent_interface_t() = default;
};

using node_parent_t = nonstd::observer_ptr<node_parent_interface_t>;

static uint id_counter;

class swayfire_t;

class node_interface_t : public virtual display_interface_t {
  protected:
    bool floating = false;
    wf::point_t wsid = {0, 0};
    wf::geometry_t geometry;
    uint node_id;
    output_ref_t output;

    node_interface_t(output_ref_t output)
        : node_id(id_counter), output(output) {
        id_counter++;
    }

  public:
    node_parent_t parent;

    split_node_ref_t as_split_node();
    view_node_ref_t as_view_node();

    virtual wf::geometry_t get_geometry() { return geometry; }
    virtual void set_geometry(wf::geometry_t geo) = 0;
    void refresh_geometry() { set_geometry(get_geometry()); }

    bool get_floating() { return floating; };
    virtual void set_floating(bool fl) = 0;

    wf::point_t get_wsid() { return wsid; };
    virtual void set_wsid(wf::point_t wsid) = 0;

    virtual node_parent_t get_active_parent_node() = 0;

    node_t find_floating_parent();

    virtual ~node_interface_t() = default;
};

class view_node_t : public node_interface_t {
  public:
    wayfire_view view;
    std::optional<split_type_t> prefered_split_type;

    view_node_t(wayfire_view view, output_ref_t output)
        : node_interface_t(output), view(view) {}

    wf::geometry_t get_geometry() override;
    void set_geometry(wf::geometry_t geo) override;
    void set_floating(bool fl) override;
    void set_wsid(wf::point_t wsid) override;
    split_node_ref_t replace_with_split();
    node_parent_t get_active_parent_node() override;

    std::ostream &to_stream(std::ostream &os) const override {
        os << "view-node-" << node_id;
        return os;
    }
};

struct view_data_t : wf::custom_data_t {
    view_node_ref_t node;

    view_data_t(view_node_ref_t node) : node(node) {}
};

class split_node_t : public node_interface_t, public node_parent_interface_t {
  private:
    std::vector<owned_node_t>::iterator find_child(node_t node);
    bool move_child_outside(node_iter_t child, direction_t dir);
    split_node_ref_t find_parent_split(bool horiz);

  public:
    split_type_t split_type = split_type_t::VSPLIT;
    uint32_t active_child = 0;
    std::vector<float> children_ratios;
    std::vector<owned_node_t> children;

    split_node_t(wf::geometry_t geo, output_ref_t output)
        : node_interface_t(output) {
        geometry = geo;
    }

    void insert_child_at(std::vector<owned_node_t>::iterator at,
                         owned_node_t node);
    void insert_child_front(owned_node_t node);
    void insert_child_back(owned_node_t node);
    void insert_child_front_of(node_t of, owned_node_t node);
    void insert_child_back_of(node_t of, owned_node_t node);
    void insert_child(owned_node_t node) override {
        insert_child_back(std::move(node));
    };

    owned_node_t remove_child(node_t node) override;
    void set_active_child(node_t node) override;

    void set_geometry(wf::geometry_t geo) override;
    void set_floating(bool fl) override;
    void set_wsid(wf::point_t wsid) override;
    node_parent_t get_active_parent_node() override;
    owned_node_t swap_child(node_t node, owned_node_t other) override;

    node_t get_last_active_node() override;

    node_t get_adjacent(node_t node, direction_t dir) override;
    bool move_child(node_t node, direction_t dir) override;

    std::ostream &to_stream(std::ostream &os) const override {
        os << "split-node-" << node_id;
        return os;
    }
};

class workspace_t : public node_parent_interface_t {
  public:
    wf::point_t wsid;
    wf::geometry_t geometry;
    std::unique_ptr<split_node_t> tiled_root;
    std::vector<owned_node_t> floating_nodes;
    output_ref_t output;

  private:
    node_t active_node;
    node_t active_tiled_node;
    uint32_t active_floating = 0;

    std::vector<owned_node_t>::iterator find_floating(node_t node);

  public:
    workspace_t(wf::point_t wsid, wf::geometry_t geo, output_ref_t output)
        : wsid(wsid), geometry(geo), output(output) {
        (void)swap_tiled_root(std::make_unique<split_node_t>(geo, output));
        LOGD("ws created with root ", tiled_root->to_string());
        active_node = tiled_root;
    };

    workspace_t(workspace_t &) = delete;
    workspace_t(workspace_t &&) = default;
    workspace_t &operator=(workspace_t &&) = default;

    void set_active_node(node_t node);
    node_t get_active_node();

    node_parent_t get_active_parent_node();

    // Floating nodes are always direct children of the workspace
    void insert_floating_node(owned_node_t node);
    owned_node_t remove_floating_node(node_t node);
    owned_node_t swap_floating_node(node_t node, owned_node_t other);
    node_t get_active_floating_node();

    owned_node_t swap_tiled_root(std::unique_ptr<split_node_t> other);

    // These methods will find the node in the workspace
    void insert_tiled_node(owned_node_t node);
    owned_node_t remove_tiled_node(node_t node);
    node_t get_active_tiled_node();
    owned_node_t remove_node(node_t node);
    void toggle_tile_node(node_t node);
    void toggle_split_direction_node(node_t node);
    node_t get_last_active_node() override;
    node_t get_adjacent(node_t node, direction_t dir) override;
    bool move_child(node_t node, direction_t dir) override;

    // These methods work with only direct children of the workspace
    // They will not walk the tree like other methods would
    void insert_child(owned_node_t node) override;
    owned_node_t remove_child(node_t node) override;
    owned_node_t swap_child(node_t node, owned_node_t other) override;
    void set_active_child(node_t node) override;

    void set_workarea(wf::geometry_t geo);
    wf::geometry_t get_geometry() { return geometry; }

    std::ostream &to_stream(std::ostream &os) const override {
        os << "workspace-" << wsid;
        return os;
    }
};

using workspace_ref_t = nonstd::observer_ptr<workspace_t>;
struct workspaces_t {
    // Workspace tree roots: workspaces[x][y]
    std::vector<std::vector<std::unique_ptr<workspace_t>>> workspaces;

    void update_dims(wf::dimensions_t ndims, wf::geometry_t geo,
                     output_ref_t output);

    workspace_ref_t get(wf::point_t ws);

    void for_each(std::function<void(workspace_ref_t)> fun);
};

class swayfire_workspace_implementation_t
    : public wf::workspace_implementation_t {
  public:
    bool view_movable(wayfire_view view) override {
        if (auto vdata = view->get_data<view_data_t>())
            return vdata->node->get_floating();

        return false;
    }

    bool view_resizable(wayfire_view view) override {
        if (auto vdata = view->get_data<view_data_t>())
            return vdata->node->get_floating();

        return false;
    }
};

inline bool is_shutting_down() {
    return wf::get_core().get_current_state() ==
           wf::compositor_state_t::SHUTDOWN;
}

class active_grab_t;
class active_button_drag_t;
class active_move_t;
class active_resize_t;

class swayfire_t : public wf::plugin_interface_t {
  private:
    workspaces_t workspaces;

    wf::option_wrapper_t<wf::keybinding_t> key_toggle_split_direction{
        "swayfire/key_toggle_split_direction"};

    wf::option_wrapper_t<wf::keybinding_t> key_set_want_vsplit{
        "swayfire/key_set_want_vsplit"};

    wf::option_wrapper_t<wf::keybinding_t> key_set_want_hsplit{
        "swayfire/key_set_want_hsplit"};

    wf::option_wrapper_t<wf::keybinding_t> key_focus_left{
        "swayfire/key_focus_left"};

    wf::option_wrapper_t<wf::keybinding_t> key_focus_right{
        "swayfire/key_focus_right"};

    wf::option_wrapper_t<wf::keybinding_t> key_focus_down{
        "swayfire/key_focus_down"};

    wf::option_wrapper_t<wf::keybinding_t> key_focus_up{
        "swayfire/key_focus_up"};

    wf::option_wrapper_t<wf::keybinding_t> key_toggle_focus_tile{
        "swayfire/key_toggle_focus_tile"};

    wf::option_wrapper_t<wf::keybinding_t> key_move_left{
        "swayfire/key_move_left"};

    wf::option_wrapper_t<wf::keybinding_t> key_move_right{
        "swayfire/key_move_right"};

    wf::option_wrapper_t<wf::keybinding_t> key_move_down{
        "swayfire/key_move_down"};

    wf::option_wrapper_t<wf::keybinding_t> key_move_up{"swayfire/key_move_up"};

    wf::option_wrapper_t<wf::keybinding_t> key_toggle_tile{
        "swayfire/key_toggle_tile"};

    wf::option_wrapper_t<wf::buttonbinding_t> button_move_activate{
        "swayfire/button_move_activate"};

    wf::option_wrapper_t<wf::buttonbinding_t> button_resize_activate{
        "swayfire/button_resize_activate"};

    std::vector<std::unique_ptr<wf::key_callback>> key_callbacks;

    bool on_toggle_split_direction(wf::keybinding_t);
    bool on_set_want_vsplit(wf::keybinding_t);
    bool on_set_want_hsplit(wf::keybinding_t);
    bool on_focus_left(wf::keybinding_t);
    bool on_focus_right(wf::keybinding_t);
    bool on_focus_down(wf::keybinding_t);
    bool on_focus_up(wf::keybinding_t);
    bool on_toggle_focus_tile(wf::keybinding_t);
    bool on_move_left(wf::keybinding_t);
    bool on_move_right(wf::keybinding_t);
    bool on_move_down(wf::keybinding_t);
    bool on_move_up(wf::keybinding_t);
    bool on_toggle_tile(wf::keybinding_t);

    wf::button_callback on_move_activate;
    wf::button_callback on_resize_activate;

    wf::signal_connection_t on_shutdown = [&](wf::signal_data_t *) {
        output->disconnect_signal(&on_view_unmapped);
    };

    wf::signal_connection_t on_view_attached = [&](wf::signal_data_t *data) {
        auto view = wf::get_signaled_view(data);

        if (view->role != wf::VIEW_ROLE_TOPLEVEL)
            return;

        auto ws = workspaces.get(nonwf::get_view_workspace(view, output));

        LOGD("attaching node in ", ws, ", ", view->to_string(), " : ",
             view->get_title());

        ws->insert_tiled_node(init_view_node(view));
    };

    wf::signal_connection_t on_view_unmapped = [&](wf::signal_data_t *data) {
        fini_view(wf::get_signaled_view(data));
    };

    wf::signal_connection_t on_view_focused = [&](wf::signal_data_t *data) {
        auto view = wf::get_signaled_view(data);
        if (auto vdata = view->get_data<view_data_t>()) {
            auto node = vdata->node;

            workspaces.get(node->get_wsid())->set_active_node(node);
        }
    };

    wf::signal_connection_t on_workarea_changed = [&](wf::signal_data_t *data) {
        auto wcdata = static_cast<wf::workarea_changed_signal *>(data);
        workspaces.for_each(
            [&](auto ws) { ws->set_workarea(wcdata->new_workarea); });
    };

    void fini_view(wayfire_view view);

    void bind_signals();
    void unbind_signals();

    void bind_keys();
    void unbind_keys();

    std::unique_ptr<view_node_t> init_view_node(wayfire_view view);

    bool focus_direction(direction_t dir);
    bool move_direction(direction_t dir);

  public:
    void init() override;
    void fini() override;
    ~swayfire_t() override;

  private:
    // grab interface
    std::unique_ptr<active_grab_t> active_grab;

    void init_grab_interface();
    void fini_grab_interface();

    friend class active_grab_t;
    friend class active_button_drag_t;
    friend class active_move_t;
    friend class active_resize_t;
};

#endif // ifndef SWAYFIRE_HPP
