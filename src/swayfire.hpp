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

/// Small wayfire helpers.
namespace nonwf {

wf::point_t get_view_workspace(wayfire_view view, OutputRef output);

/// Convert geo from from_wsid to to_wsid coordinate space.
wf::geometry_t local_to_relative_geometry(wf::geometry_t geo,
                                          wf::point_t from_wsid,
                                          wf::point_t to_wsid,
                                          OutputRef output);

/// Get the center point of a geo.
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

/// Return the direction opposite to dir.
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
using SplitNodeRef = nonstd::observer_ptr<SplitNode>;
using ViewNodeRef = nonstd::observer_ptr<ViewNode>;

using NodeIter = std::vector<OwnedNode>::iterator;

/// Interface for display-able types.
class IDisplay {
  public:
    [[nodiscard]] virtual std::string to_string() const {
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

/// Interface for common functionality of node parents.
///
/// Node parents are not necessarily a nodes themselves.
class INodeParent : public virtual IDisplay {
  public:
    /// Dynamic cast to SplitNodeRef.
    SplitNodeRef as_split_node();

    /// Find the node directly adjacent to node in the given direction.
    ///
    /// This can traverse parents upwards in order to find the adjacent node,
    /// but does not guarantee returning a ViewNode and so does not traverse the
    /// tree downwards at all.
    virtual Node get_adjacent(Node node, Direction dir) = 0;

    /// Move a direct child of this parent in the given direction.
    ///
    /// The child may be moved upward in the tree in order to find an adjacent
    /// slot in the given direction. The child may also be moved deeper into the
    /// tree if the adjacent node in the given direction is a split.
    ///
    /// \return True if the child was moved.
    virtual bool move_child(Node node, Direction dir) = 0;

    /// Get the deepest last active child node.
    ///
    /// The returned node may be an indirect child of this parent.
    virtual Node get_last_active_node() = 0;

    /// Insert a new direct child into this parent.
    virtual void insert_child(OwnedNode node) = 0;

    /// Remove a direct child from this parent.
    virtual OwnedNode remove_child(Node node) = 0;

    /// Swap a direct child of this parent with some other node.
    virtual OwnedNode swap_child(Node node, OwnedNode other) = 0;

    /// Set the last active direct child of this parent and set this parent to
    /// be the last active child of its parent.
    ///
    /// This call should bubble up to the root parent.
    virtual void set_active_child(Node node) = 0;
};

using NodeParent = nonstd::observer_ptr<INodeParent>;

/// Id counter for generating node ids
static uint id_counter;

class Swayfire;

/// Interface for common functionality of nodes.
class INode : public virtual IDisplay {
  protected:
    /// Whether this node is floating.
    ///
    /// If this node is a parent only it is considered floating and not its
    /// children.
    bool floating = false;

    wf::point_t wsid = {0, 0}; ///< The workspace by which this node is managed.
    wf::geometry_t geometry;   ///< The outer geometry of this node.
    uint node_id;              ///< The id of this node.

    OutputRef output; ///< The wayfire output by which this node is managed.

    INode(OutputRef output) : node_id(id_counter), output(output) {
        id_counter++;
    }

  public:
    NodeParent parent; ///< The parent of this node.

    /// Dynamic cast to SplitNodeRef.
    SplitNodeRef as_split_node();

    /// Dynamic cast to ViewNodeRef.
    ViewNodeRef as_view_node();

    /// Get the outer geometry of the node.
    virtual wf::geometry_t get_geometry() { return geometry; }

    /// Set the outer geometry of the node.
    ///
    /// This call can cause the geometry of children nodes to be updated as
    /// well. This call does not bubble upwards however.
    virtual void set_geometry(wf::geometry_t geo) = 0;

    /// Set the outer geometry of the node to its current value.
    ///
    /// This is mainly to cause a recalculation of children geometries.
    void refresh_geometry() { set_geometry(get_geometry()); }

    /// Resize outer geometry to ndims if possible with locked_edges.
    ///
    /// The locked edges remain in place while the others move to achieve the
    /// requested dimensions. This may be a noop: if both right and left edges
    /// are locked for example, the new width dimension will not be applied.
    virtual void try_resize(wf::dimensions_t ndims, uint32_t locked_edges);

    /// Get whether this node is floating.
    bool get_floating() { return floating; };

    /// Set whether this node is floating.
    virtual void set_floating(bool fl) = 0;

    /// Get the workspace that manages this node.
    wf::point_t get_wsid() { return wsid; };

    /// Set the workspace that manages this node.
    virtual void set_wsid(wf::point_t wsid) = 0;

    /// Return self if this node is a parent or try to upgrade this node to
    /// become a parent or return the parent of this node.
    virtual NodeParent get_or_upgrade_to_parent_node() = 0;

    /// Return this node if it's floating or traverse the tree upward to find a
    /// floating parent.
    Node find_floating_parent();
};

/// A node corresponding to a wayfire view.
class ViewNode : public INode {
  public:
    /// The wayfire view corresponding to this node.
    wayfire_view view;

    /// The prefered split type for upgrading this node to a split node.
    std::optional<SplitType> prefered_split_type;

    ViewNode(wayfire_view view, OutputRef output) : INode(output), view(view) {}

    /// Try to upgrade this node to a split node.
    ///
    /// A view node is only upgradable to a split if a split preference is set.
    /// When upgraded, the node swaps itself in its parent for the created split
    /// node and adds itself to this split node. Finally, the split preference
    /// is cleared.
    SplitNodeRef try_upgrade();

    // == INode impl ==

    wf::geometry_t get_geometry() override;
    void set_geometry(wf::geometry_t geo) override;
    void set_floating(bool fl) override;
    void set_wsid(wf::point_t wsid) override;
    NodeParent get_or_upgrade_to_parent_node() override;

    // == IDisplay impl ==

    std::ostream &to_stream(std::ostream &os) const override {
        os << "view-node-" << node_id;
        return os;
    }
};

/// The custom data attached to wayfire views to point to the corresponding view
/// node.
struct ViewData : wf::custom_data_t {
    /// Pointer to the corresponding view node.
    ViewNodeRef node;

    ViewData(ViewNodeRef node) : node(node) {}
};

/// A split node containing children.
class SplitNode : public INode, public INodeParent {
  private:
    /// Find a direct child of this parent node.
    NodeIter find_child(Node node);

    /// Move a direct child outside of this parent in the given direction.
    ///
    /// This either moves the node into an adjacent parent node or at the
    /// back/front of an (in)direct parent.
    bool move_child_outside(NodeIter child, Direction dir);

    /// Walk up the tree to find the first split node parent that is (not)
    /// horizontal.
    SplitNodeRef find_parent_split(bool horiz);

  public:
    SplitType split_type = SplitType::VSPLIT; ///< The split type of this node.
    uint32_t active_child = 0;                ///< Index of last active child.
    std::vector<float> children_ratios;       ///< The Size ratios of children.
    std::vector<OwnedNode> children;          ///< The direct children nodes.

    SplitNode(wf::geometry_t geo, OutputRef output) : INode(output) {
        geometry = geo;
    }

    /// Insert a direct child at the given position in children.
    void insert_child_at(NodeIter at, OwnedNode node);

    /// Insert a direct child at the front of children.
    void insert_child_front(OwnedNode node);

    /// Insert a direct child at the back of children.
    void insert_child_back(OwnedNode node);

    /// Insert a direct child just before another direct child.
    void insert_child_front_of(Node of, OwnedNode node);

    /// Insert a direct child just after another direct child.
    void insert_child_back_of(Node of, OwnedNode node);

    /// Toggle the split direction of this node.
    void toggle_split_direction();

    // == INodeParent impl ==

    Node get_adjacent(Node node, Direction dir) override;
    bool move_child(Node node, Direction dir) override;
    Node get_last_active_node() override;
    void insert_child(OwnedNode node) override {
        insert_child_back(std::move(node));
    };
    OwnedNode remove_child(Node node) override;
    OwnedNode swap_child(Node node, OwnedNode other) override;
    void set_active_child(Node node) override;

    // == INode impl ==

    void set_geometry(wf::geometry_t geo) override;
    void set_floating(bool fl) override;
    void set_wsid(wf::point_t wsid) override;
    NodeParent get_or_upgrade_to_parent_node() override;

    // == IDisplay impl ==

    std::ostream &to_stream(std::ostream &os) const override {
        os << "split-node-" << node_id;
        return os;
    }
};

/// A single workspace managing a tiled tree and floating nodes.
class Workspace : public INodeParent {
  public:
    /// The workarea of this ws.
    ///
    /// The workarea is the output size minus space reserves for panels and
    /// such.
    wf::geometry_t workarea;

    /// The position of this ws on the ws grid.
    wf::point_t wsid;

    /// The tiled tree that fills this workspace.
    std::unique_ptr<SplitNode> tiled_root;

    /// The floating nodes that are manages by this ws.
    ///
    /// All floating nodes are direct children of their workspace.
    std::vector<OwnedNode> floating_nodes;

    /// The wayfire output that this workspace is on.
    OutputRef output;

  private:
    /// Reference to the node currently active in this ws.
    Node active_node;

    /// Reference to the tiled node last active in this ws.
    Node active_tiled_node;

    /// The last active floating node index.
    uint32_t active_floating = 0;

    /// Find a floating child of this ws.
    NodeIter find_floating(Node node);

  public:
    Workspace(wf::point_t wsid, wf::geometry_t geo, OutputRef output)
        : workarea(geo), wsid(wsid), output(output) {
        (void)swap_tiled_root(std::make_unique<SplitNode>(geo, output));
        LOGD("ws created with root ", tiled_root->to_string());
        active_node = tiled_root;
    };

    Workspace(Workspace &) = delete;
    Workspace(Workspace &&) = default;
    Workspace &operator=(Workspace &&) = default;

    /// Set the currently active node in this ws.
    void set_active_node(Node node);

    /// Get the currently active node in this ws.
    Node get_active_node();

    /// Set the workarea of the workspace.
    void set_workarea(wf::geometry_t geo);

    /// Get the workarea of the workspace.
    wf::geometry_t get_workarea() { return workarea; }

    // == Floating ==

    /// Insert a floating node into this workspace.
    void insert_floating_node(OwnedNode node);

    /// Remove a floating node from this workspace.
    OwnedNode remove_floating_node(Node node);

    /// Swap a floating node in this workspace for another node.
    OwnedNode swap_floating_node(Node node, OwnedNode other);

    /// Get the last active floating node in this ws.
    Node get_active_floating_node();

    // == Tiled ==

    /// Insert a tiled node into this ws.
    void insert_tiled_node(OwnedNode node);

    /// Remove a tiled node from this ws.
    OwnedNode remove_tiled_node(Node node);

    /// Swap the root tiled split node with another.
    OwnedNode swap_tiled_root(std::unique_ptr<SplitNode> other);

    /// Get the last active tiled node in this ws.
    Node get_active_tiled_node();

    // == Both ==

    /// Remove a node from this ws.
    OwnedNode remove_node(Node node);

    /// Toggle tiling on a ndoe in this ws.
    void toggle_tile_node(Node node);

    // == INodeParent impl ==

    Node get_adjacent(Node node, Direction dir) override;
    bool move_child(Node node, Direction dir) override;
    Node get_last_active_node() override;
    void insert_child(OwnedNode node) override;
    OwnedNode remove_child(Node node) override;
    OwnedNode swap_child(Node node, OwnedNode other) override;
    void set_active_child(Node node) override;

    // == IDisplay impl ==

    std::ostream &to_stream(std::ostream &os) const override {
        os << "workspace-" << wsid;
        return os;
    }
};

using WorkspaceRef = nonstd::observer_ptr<Workspace>;

/// Grid of all the workspaces on an output.
struct Workspaces {
    /// Workspace tree roots: workspaces[x][y].
    std::vector<std::vector<std::unique_ptr<Workspace>>> workspaces;

    /// Update the dimensions of the workspace grid.
    void update_dims(wf::dimensions_t ndims, wf::geometry_t geo,
                     OutputRef output);

    /// Get the workspace at the given coordinate in the grid.
    WorkspaceRef get(wf::point_t ws);

    /// Iterate through all workspaces in the grid.
    void for_each(const std::function<void(WorkspaceRef)> &fun);
};

/// Custom wayfire workspace implementation.
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

/// Get whether wayfire is currently shutting down.
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
    /// The workspaces manages by swayfire.
    Workspaces workspaces;

    /// Stores all the key callbacks bound.
    std::vector<std::unique_ptr<wf::key_callback>> key_callbacks;

    /// The current active gesture grab.
    std::unique_ptr<IActiveGrab> active_grab;

    /// Bind all signal handlers needed.
    void bind_signals();

    /// Unbind all signal handlers bound.
    void unbind_signals();

    /// Bind all key callbacks needed.
    void bind_keys();

    /// Unbind all key callbacks bound.
    void unbind_keys();

    /// Make a new view_node corresponding to the given view.
    std::unique_ptr<ViewNode> init_view_node(wayfire_view view);

    /// Destroy the view node corresponging to the given view.
    void fini_view(wayfire_view view);

    /// Initialize gesture grab interfaces and activators.
    void init_grab_interface();

    /// Destroy gesture grab interfaces and activators.
    void fini_grab_interface();

    friend class IActiveGrab;
    friend class IActiveButtonDrag;
    friend class ActiveMove;
    friend class ActiveResize;

    // == Bindings and Binding Callbacks ==

    /// Focus the node in the given direction from the active node.
    bool focus_direction(Direction dir);

    /// Move the active node in the given direction.
    bool move_direction(Direction dir);

#define DECL_KEY(NAME)                                                         \
    wf::option_wrapper_t<wf::keybinding_t> key_##NAME{"swayfire/key_" #NAME};  \
    bool on_##NAME(wf::keybinding_t);

    DECL_KEY(toggle_split_direction);
    DECL_KEY(set_want_vsplit);
    DECL_KEY(set_want_hsplit);
    DECL_KEY(focus_left);
    DECL_KEY(focus_right);
    DECL_KEY(focus_down);
    DECL_KEY(focus_up);
    DECL_KEY(toggle_focus_tile);
    DECL_KEY(move_left);
    DECL_KEY(move_right);
    DECL_KEY(move_down);
    DECL_KEY(move_up);
    DECL_KEY(toggle_tile);
#undef DECL_KEY

    wf::option_wrapper_t<wf::buttonbinding_t> button_move_activate{
        "swayfire/button_move_activate"};

    wf::option_wrapper_t<wf::buttonbinding_t> button_resize_activate{
        "swayfire/button_resize_activate"};

    wf::button_callback on_move_activate;
    wf::button_callback on_resize_activate;

    // == Signal Handlers == //

    wf::signal_connection_t on_shutdown = [&](wf::signal_data_t *) {
        output->disconnect_signal(&on_view_unmapped);
    };

    /// Handle new created views.
    wf::signal_connection_t on_view_attached = [&](wf::signal_data_t *data) {
        auto view = wf::get_signaled_view(data);

        if (view->role != wf::VIEW_ROLE_TOPLEVEL)
            return;

        auto ws = workspaces.get(nonwf::get_view_workspace(view, output));

        LOGD("attaching node in ", ws, ", ", view->to_string(), " : ",
             view->get_title());

        ws->insert_tiled_node(init_view_node(view));
    };

    /// Handle destroyed views.
    wf::signal_connection_t on_view_unmapped = [&](wf::signal_data_t *data) {
        fini_view(wf::get_signaled_view(data));
    };

    /// Handle focused view changes.
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

  public:
    // == Impl wf::plugin_interface_t ==

    void init() override;
    void fini() override;
    ~Swayfire() override;
};

#endif // ifndef SWAYFIRE_HPP
