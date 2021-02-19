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

using OutputRef = nonstd::observer_ptr<wf::output_t>;

namespace nonwf {

wf::point_t get_view_workspace(wayfire_view view, OutputRef output);

// Convert workspace-local geo to relative-to-current-workspace geo
wf::geometry_t local_to_relative_geometry(wf::geometry_t geo, wf::point_t wsid,
                                          OutputRef output);

wf::point_t geometry_center(wf::geometry_t geo);

} // namespace nonwf

enum struct SplitType : uint8_t {
    VSPLIT,
    HSPLIT,
    TABBED,
    STACKED,
};

enum struct Direction : uint8_t {
    UP,
    DOWN,
    LEFT,
    RIGHT,
};

inline Direction opposite_dir(Direction dir) {
    switch (dir) {
    case Direction::LEFT:
        return Direction::RIGHT;
    case Direction::RIGHT:
        return Direction::LEFT;

    case Direction::DOWN:
        return Direction::UP;
    case Direction::UP:
        return Direction::DOWN;
    }
}

class INode;
class SplitNode;
class ViewNode;

using OwnedNode = std::unique_ptr<INode>;
using Node = nonstd::observer_ptr<INode>;
using NodeIter = std::vector<OwnedNode>::iterator;

using SplitNodeRef = nonstd::observer_ptr<SplitNode>;
using ViewNodeRef = nonstd::observer_ptr<ViewNode>;

class IDisplay {
  public:
    virtual std::string to_string() const {
        std::ostringstream out;
        out << this;
        return out.str();
    };
    virtual std::ostream &to_stream(std::ostream &os) const = 0;

    virtual ~IDisplay() = default;
};

inline std::ostream &operator<<(std::ostream &os, const IDisplay &n) {
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

class INodeParent : public virtual IDisplay {
  public:
    SplitNodeRef as_split_node();

    virtual Node get_adjacent(Node node, Direction dir) = 0;
    virtual bool move_child(Node node, Direction dir) = 0;

    virtual Node get_last_active_node() = 0;

    virtual void insert_child(OwnedNode node) = 0;
    virtual OwnedNode remove_child(Node node) = 0;
    virtual OwnedNode swap_child(Node node, OwnedNode other) = 0;
    virtual void set_active_child(Node node) = 0;

    virtual ~INodeParent() = default;
};

using NodeParent = nonstd::observer_ptr<INodeParent>;

static uint id_counter;

class Swayfire;

class INode : public virtual IDisplay {
  protected:
    bool floating = false;
    wf::point_t wsid = {0, 0};
    wf::geometry_t geometry;
    uint node_id;
    OutputRef output;

    INode(OutputRef output) : node_id(id_counter), output(output) {
        id_counter++;
    }

  public:
    NodeParent parent;

    SplitNodeRef as_split_node();
    ViewNodeRef as_view_node();

    virtual wf::geometry_t get_geometry() { return geometry; }
    virtual void set_geometry(wf::geometry_t geo) = 0;
    void refresh_geometry() { set_geometry(get_geometry()); }

    bool get_floating() { return floating; };
    virtual void set_floating(bool fl) = 0;

    wf::point_t get_wsid() { return wsid; };
    virtual void set_wsid(wf::point_t wsid) = 0;

    virtual NodeParent get_active_parent_node() = 0;

    Node find_floating_parent();

    virtual ~INode() = default;
};

class ViewNode : public INode {
  public:
    wayfire_view view;
    std::optional<SplitType> prefered_split_type;

    ViewNode(wayfire_view view, OutputRef output) : INode(output), view(view) {}

    wf::geometry_t get_geometry() override;
    void set_geometry(wf::geometry_t geo) override;
    void set_floating(bool fl) override;
    void set_wsid(wf::point_t wsid) override;
    SplitNodeRef replace_with_split();
    NodeParent get_active_parent_node() override;

    std::ostream &to_stream(std::ostream &os) const override {
        os << "view-node-" << node_id;
        return os;
    }
};

struct ViewData : wf::custom_data_t {
    ViewNodeRef node;

    ViewData(ViewNodeRef node) : node(node) {}
};

class SplitNode : public INode, public INodeParent {
  private:
    NodeIter find_child(Node node);
    bool move_child_outside(NodeIter child, Direction dir);
    SplitNodeRef find_parent_split(bool horiz);

  public:
    SplitType split_type = SplitType::VSPLIT;
    uint32_t active_child = 0;
    std::vector<float> children_ratios;
    std::vector<OwnedNode> children;

    SplitNode(wf::geometry_t geo, OutputRef output) : INode(output) {
        geometry = geo;
    }

    void insert_child_at(std::vector<OwnedNode>::iterator at, OwnedNode node);
    void insert_child_front(OwnedNode node);
    void insert_child_back(OwnedNode node);
    void insert_child_front_of(Node of, OwnedNode node);
    void insert_child_back_of(Node of, OwnedNode node);
    void insert_child(OwnedNode node) override {
        insert_child_back(std::move(node));
    };

    OwnedNode remove_child(Node node) override;
    void set_active_child(Node node) override;

    void set_geometry(wf::geometry_t geo) override;
    void set_floating(bool fl) override;
    void set_wsid(wf::point_t wsid) override;
    NodeParent get_active_parent_node() override;
    OwnedNode swap_child(Node node, OwnedNode other) override;

    Node get_last_active_node() override;

    Node get_adjacent(Node node, Direction dir) override;
    bool move_child(Node node, Direction dir) override;

    std::ostream &to_stream(std::ostream &os) const override {
        os << "split-node-" << node_id;
        return os;
    }
};

class Workspace : public INodeParent {
  public:
    wf::point_t wsid;
    wf::geometry_t geometry;
    std::unique_ptr<SplitNode> tiled_root;
    std::vector<OwnedNode> floating_nodes;
    OutputRef output;

  private:
    Node active_node;
    Node active_tiled_node;
    uint32_t active_floating = 0;

    std::vector<OwnedNode>::iterator find_floating(Node node);

  public:
    Workspace(wf::point_t wsid, wf::geometry_t geo, OutputRef output)
        : wsid(wsid), geometry(geo), output(output) {
        (void)swap_tiled_root(std::make_unique<SplitNode>(geo, output));
        LOGD("ws created with root ", tiled_root->to_string());
        active_node = tiled_root;
    };

    Workspace(Workspace &) = delete;
    Workspace(Workspace &&) = default;
    Workspace &operator=(Workspace &&) = default;

    void set_active_node(Node node);
    Node get_active_node();

    NodeParent get_active_parent_node();

    // Floating nodes are always direct children of the workspace
    void insert_floating_node(OwnedNode node);
    OwnedNode remove_floating_node(Node node);
    OwnedNode swap_floating_node(Node node, OwnedNode other);
    Node get_active_floating_node();

    OwnedNode swap_tiled_root(std::unique_ptr<SplitNode> other);

    // These methods will find the node in the workspace
    void insert_tiled_node(OwnedNode node);
    OwnedNode remove_tiled_node(Node node);
    Node get_active_tiled_node();
    OwnedNode remove_node(Node node);
    void toggle_tile_node(Node node);
    void toggle_split_direction_node(Node node);
    Node get_last_active_node() override;
    Node get_adjacent(Node node, Direction dir) override;
    bool move_child(Node node, Direction dir) override;

    // These methods work with only direct children of the workspace
    // They will not walk the tree like other methods would
    void insert_child(OwnedNode node) override;
    OwnedNode remove_child(Node node) override;
    OwnedNode swap_child(Node node, OwnedNode other) override;
    void set_active_child(Node node) override;

    void set_workarea(wf::geometry_t geo);
    wf::geometry_t get_geometry() { return geometry; }

    std::ostream &to_stream(std::ostream &os) const override {
        os << "workspace-" << wsid;
        return os;
    }
};

using WorkspaceRef = nonstd::observer_ptr<Workspace>;
struct Workspaces {
    // Workspace tree roots: workspaces[x][y]
    std::vector<std::vector<std::unique_ptr<Workspace>>> workspaces;

    void update_dims(wf::dimensions_t ndims, wf::geometry_t geo,
                     OutputRef output);

    WorkspaceRef get(wf::point_t ws);

    void for_each(std::function<void(WorkspaceRef)> fun);
};

class SwayfireWorkspaceImpl : public wf::workspace_implementation_t {
  public:
    bool view_movable(wayfire_view view) override {
        if (auto vdata = view->get_data<ViewData>())
            return vdata->node->get_floating();

        return false;
    }

    bool view_resizable(wayfire_view view) override {
        if (auto vdata = view->get_data<ViewData>())
            return vdata->node->get_floating();

        return false;
    }
};

inline bool is_shutting_down() {
    return wf::get_core().get_current_state() ==
           wf::compositor_state_t::SHUTDOWN;
}

class IActiveGrab;
class IActiveButtonDrag;
class ActiveMove;
class ActiveResize;

class Swayfire : public wf::plugin_interface_t {
  private:
    Workspaces workspaces;

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
        if (auto vdata = view->get_data<ViewData>()) {
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

    std::unique_ptr<ViewNode> init_view_node(wayfire_view view);

    bool focus_direction(Direction dir);
    bool move_direction(Direction dir);

  public:
    void init() override;
    void fini() override;
    ~Swayfire() override;

  private:
    // grab interface
    std::unique_ptr<IActiveGrab> active_grab;

    void init_grab_interface();
    void fini_grab_interface();

    friend class IActiveGrab;
    friend class IActiveButtonDrag;
    friend class ActiveMove;
    friend class ActiveResize;
};

#endif // ifndef SWAYFIRE_HPP
