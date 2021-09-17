#ifndef SWAYFIRE_CORE_HPP
#define SWAYFIRE_CORE_HPP

#include <cassert>
#include <cstdint>
#include <memory>
#include <optional>
#include <sys/types.h>
#include <vector>

#include <wayfire/config/types.hpp>
#include <wayfire/core.hpp>
#include <wayfire/geometry.hpp>
#include <wayfire/nonstd/observer_ptr.h>
#include <wayfire/object.hpp>
#include <wayfire/option-wrapper.hpp>
#include <wayfire/output.hpp>
#include <wayfire/plugin.hpp>
#include <wayfire/signal-definitions.hpp>
#include <wayfire/util/log.hpp>
#include <wayfire/view-transform.hpp>
#include <wayfire/workspace-manager.hpp>

#include "signals.hpp"

constexpr std::uint32_t FLOATING_MOVE_STEP = 5;
constexpr std::int32_t MIN_VIEW_SIZE = 20;

using OutputRef = nonstd::observer_ptr<wf::output_t>;

/// Small wayfire helpers.
namespace nonwf {

/// Get the wsid of the workspace on which most of the view is visible.
wf::point_t get_view_workspace(wayfire_view view, bool with_transform = false);

/// Convert geo from from_wsid to to_wsid coordinate space.
wf::geometry_t local_to_relative_geometry(wf::geometry_t geo,
                                          wf::point_t from_wsid,
                                          wf::point_t to_wsid,
                                          OutputRef output);

/// Get the center point of a geo.
wf::point_t geometry_center(wf::geometry_t geo);

/// Apply std::min on both components of the two dimensions independently.
wf::dimensions_t min(const wf::dimensions_t &a, const wf::dimensions_t &b);

/// Apply std::max on both components of the two dimensions independently.
wf::dimensions_t max(const wf::dimensions_t &a, const wf::dimensions_t &b);

constexpr std::uint32_t ALL_EDGES =
    (WLR_EDGE_LEFT | WLR_EDGE_RIGHT | WLR_EDGE_TOP | WLR_EDGE_BOTTOM);

} // namespace nonwf

enum struct SplitType : std::uint8_t {
    VSPLIT,
    HSPLIT,
    TABBED,
    STACKED,
};

enum struct Direction : std::uint8_t {
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
class Workspace;

using OwnedNode = std::unique_ptr<INode>;
using Node = nonstd::observer_ptr<INode>;
using SplitNodeRef = nonstd::observer_ptr<SplitNode>;
using ViewNodeRef = nonstd::observer_ptr<ViewNode>;
using WorkspaceRef = nonstd::observer_ptr<Workspace>;

using NodeIter = std::vector<OwnedNode>::iterator;

/// Interface for display-able types.
class IDisplay {
  public:
    [[nodiscard]] virtual std::string to_string() const {
        std::ostringstream out;
        to_stream(out);
        return out.str();
    };
    virtual std::ostream &to_stream(std::ostream &os) const = 0;

    virtual ~IDisplay() = default;
};

inline std::ostream &operator<<(std::ostream &os, const IDisplay &n) {
    return n.to_stream(os);
}

inline std::ostream &operator<<(std::ostream &os, const IDisplay *n) {
    if (n)
        return n->to_stream(os);
    os << "(null)";
    return os;
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
    /// Prefer calling INode::move() which calls this method internally.
    ///
    /// \return True if the child was moved.
    virtual bool move_child(Node node, Direction dir) = 0;

    /// Try to resize the child to the given dimensions by moving the given
    /// edges.
    ///
    /// This call bubbles upward resizing this split in its parent if this split
    /// cannot contain the new dimensions.
    ///
    /// \return the new dimensions after this call.
    virtual wf::dimensions_t try_resize_child(Node child,
                                              wf::dimensions_t ndims,
                                              std::uint32_t edges) = 0;

    /// Get the deepest last active child node.
    ///
    /// The returned node may be an indirect child of this parent or even this
    /// parent itself.
    virtual Node get_last_active_node() = 0;

    /// Insert a new direct child into this parent.
    virtual void insert_child(OwnedNode node) = 0;

    /// Remove a direct child from this parent.
    virtual OwnedNode remove_child(Node node) = 0;

    /// Swap a direct child of this parent with some other node.
    virtual OwnedNode swap_child(Node node, OwnedNode other) = 0;

    /// Swap two direct children of this parent.
    virtual void swap_children(Node a, Node b) = 0;

    /// Set the last active direct child of this parent and set this parent to
    /// be the last active child of its parent.
    ///
    /// This call should bubble up to the root parent.
    virtual void set_active_child(Node node) = 0;

    /// Get this parent's last active child.
    virtual Node get_active_child() const = 0;

    /// Notify this parent that a direct child has changed its title.
    virtual void notify_child_title_changed(Node child) { (void)child; }
};

using NodeParent = nonstd::observer_ptr<INodeParent>;

/// Id counter for generating node ids
static uint id_counter;

class Swayfire;

struct Padding {
    int left, right, top, bottom;

    Padding &operator+=(const Padding &other) {
        left += other.left;
        right += other.right;
        top += other.top;
        bottom += other.bottom;
        return *this;
    }

    Padding &operator-=(const Padding &other) {
        left -= other.left;
        right -= other.right;
        top -= other.top;
        bottom -= other.bottom;
        return *this;
    }

    Padding operator-() const { return {-left, -right, -top, -bottom}; }

    friend Padding operator+(Padding a, const Padding &b) {
        a += b;
        return a;
    }

    friend Padding operator-(Padding a, const Padding &b) {
        a -= b;
        return a;
    }

    /// Return false if the padding is empty.
    operator bool() { return left || right || top || bottom; }

    /// Add the padding area to the geometry.
    friend wf::geometry_t operator+(wf::geometry_t a, const Padding &b) {
        a.x -= b.left;
        a.y -= b.top;
        a.width += b.left + b.right;
        a.height += b.top + b.bottom;
        return a;
    }

    /// Subtract the padding area from the geometry.
    friend wf::geometry_t operator-(wf::geometry_t a, const Padding &b) {
        return a + (-b);
    }
};

/// Interface for common functionality of nodes.
class INode : public virtual IDisplay, public wf::object_base_t {
  protected:
    /// Whether this node is floating.
    ///
    /// If this node is a parent only *it* is considered floating and not its
    /// children.
    bool floating = false;

    /// The last floating geometry of this node.
    wf::geometry_t floating_geometry;

    WorkspaceRef ws = nullptr; ///< The workspace by which this node is managed.
    wf::geometry_t geometry;   ///< The outer geometry of this node.

    /// The accumulated padding space around the node's inner_geometry.
    ///
    /// Padding is by convention expected to be space allocated for subsurfaces
    /// of the node that will stick to its sides.
    Padding padding{};

    /// Expand the given geometry treating it as the inner geometry of this node
    /// to get the outer geometry.
    wf::geometry_t expand_geometry(wf::geometry_t geo) { return geo + padding; }

    uint node_id; ///< The id of this node.

    uint pure_set_geo = 0; ///< If non-zero, disables side-effects of
                           ///< set_geometry().

    /// Views attached to this node. Equivalent of view subsurfaces, but for
    /// nodes.
    std::vector<wayfire_view> subsurfaces;

    /// Whether this node has been fully initialized yet.
    bool initialized = false;

    /// Handle this node having been initialized.
    virtual void on_initialized() = 0;

    /// Close all the subsurfaces of this node.
    void close_subsurfaces();

    /// Emit the "title-changed" signal and propagate it to the parent nodes.
    void emit_title_changed();

    INode() : node_id(id_counter) { id_counter++; }

  public:
    /// Prefered geo for the node.
    ///
    /// This get's set at the beginning of a continuous resize.
    /// This currently gets used as a maximum preferred size during continuous
    /// resizes.
    std::optional<wf::dimensions_t> preferred_size = std::nullopt;

    NodeParent parent; ///< The parent of this node.

    /// Dynamic cast to SplitNodeRef.
    SplitNodeRef as_split_node();

    /// Dynamic cast to ViewNodeRef.
    ViewNodeRef as_view_node();

    /// Notify the node that it has been initialized.
    ///
    /// Noop if node is already initialized
    void notify_initialized();

    /// Get the node's title.
    virtual std::string get_title() = 0;

    /// Get the outer geometry of the node.
    wf::geometry_t get_geometry() { return geometry; }

    /// Get the inner geometry of the node.
    wf::geometry_t get_inner_geometry() { return geometry - padding; }

    /// Set the outer geometry of the node.
    ///
    /// This call can cause the geometry of children nodes to be updated as
    /// well. This call does not bubble upwards however.
    virtual void set_geometry(wf::geometry_t geo) = 0;

    /// Set the outer geometry of the node to its current value.
    ///
    /// This is mainly to cause a recalculation of children geometries.
    void refresh_geometry() { set_geometry(get_geometry()); }

    /// Increment the pure set_geometry() mode reference count and prevent
    /// side-effects.
    void ref_pure_set_geo() { pure_set_geo++; }

    /// Decrement the pure set_geometry() mode reference count and allow
    /// side-effects if count is 0.
    void unref_pure_set_geo() {
        assert(pure_set_geo);
        pure_set_geo--;
    }

    /// Add the given padding to this node.
    ///
    /// Add negative padding to remove from the current padding.
    /// This function does not refresh_geometry() for you.
    void add_padding(Padding padding);

    /// Get this node's padding.
    Padding get_padding();

    /// Add a new subsurface to this node.
    void add_subsurface(wayfire_view subsurf);

    /// Remove a subsurface from this node.
    void remove_subsurface(wayfire_view subsurf);

    /// Resize outer geometry to ndims if possible --by moving the given edges.
    ///
    /// The other edges remain in place while the moving edges move to achieve
    /// the requested dimensions. This may be a noop: if neither the right or
    /// left edges are moving for example, the new width dimension will not be
    /// applied.
    ///
    /// \return the new dimensions after this call.
    virtual wf::dimensions_t try_resize(wf::dimensions_t ndims,
                                        std::uint32_t edges);

    /// Begin a continuous resize on this node and its children.
    virtual void begin_resize() {
        preferred_size = wf::dimensions(get_geometry());
    };

    /// End a continuous resize on this node and its children.
    virtual void end_resize() { preferred_size = std::nullopt; };

    /// Get whether this node is floating.
    bool get_floating() { return floating; };

    /// Set whether this node is floating.
    virtual void set_floating(bool fl);

    /// Get the workspace that manages this node.
    WorkspaceRef get_ws() { return ws; };

    /// Set the workspace that manages this node.
    virtual void set_ws(WorkspaceRef ws) {
        assert(ws);
        this->ws = ws;
    };

    /// Set the sublayer of views in the subtree starting at this node.
    virtual void set_sublayer(nonstd::observer_ptr<wf::sublayer_t> sublayer);

    /// Bring this node's whole tree to the foreground.
    virtual void bring_to_front();

    /// Make this node the active selected node in its workspace.
    void set_active();

    /// Handle this node being made the active node of its workspace.
    virtual void on_set_active() {}

    /// Try to (un)tile this node in its workspace.
    void tile_request(bool tile);

    /// Move this node in the given direction within its tree.
    ///
    /// See INodeParent::move_child() for in-depth behaviour.
    ///
    /// \return whether we were able to move in the given direction.
    bool move(Direction dir);

    /// Return self if this node is a parent or try to upgrade this node to
    /// become a parent or return the parent of this node.
    virtual NodeParent get_or_upgrade_to_parent_node() = 0;

    /// Return this node if it's a direct child of its workspace or traverse the
    /// tree upward to find the root parent node.
    Node find_root_parent();

    /// Return this node if it's floating or traverse the tree upward to find a
    /// floating parent.
    ///
    /// \return a floating Node or nullptr if one isn't found.
    Node find_floating_parent();

    /// Apply the given function over all nodes of this tree including this
    /// node. (Pre-order traversal)
    virtual void for_each_node(const std::function<void(Node)> &f) = 0;
};

/// Transformer to force views to their supposed geometries.
///
/// This is a temporary workaround for
/// https://github.com/Javyre/swayfire/issues/1.
///
/// Currently waiting on https://github.com/WayfireWM/wayfire/issues/995 which
/// is planned for wayfire 0.9.
class ViewGeoEnforcer final : public wf::view_2D {
  private:
    ViewNodeRef view_node;

    /// Reference counted geo_enforcer disable switch.
    uint disabled = 0;

    /// Handle the view changing geometry.
    wf::signal_connection_t on_geometry_changed = [&](wf::signal_data_t *) {
        update_transformer();
    };

  public:
    ViewGeoEnforcer(ViewNodeRef node);

    ~ViewGeoEnforcer() override;

    /// Increment the disable reference count and disable the geo enforcer.
    void ref_disable();

    /// Decrement the disable reference count and enable the geo enforcer if
    /// count is 0.
    void unref_disable();

    /// Update the scaling and offset to enforce the geometry.
    void update_transformer();
};

struct ViewData;

/// A node corresponding to a wayfire view.
class ViewNode final : public INode {
    friend ViewGeoEnforcer;

  private:
    /// The prefered split type for upgrading this node to a split node.
    std::optional<SplitType> prefered_split_type = std::nullopt;

    /// Handle the view being mapped.
    wf::signal_connection_t on_mapped = [&](wf::signal_data_t *) {
        if (view->tiled_edges != wf::TILED_EDGES_ALL)
            floating_geometry = expand_geometry(view->get_wm_geometry());
    };

    /// Handle unmapped views.
    wf::signal_connection_t on_unmapped = [&](wf::signal_data_t *) {
        // can't inline it here since depends on ws methods.
        on_unmapped_impl();
    };

    /// Handle geometry changes.
    wf::signal_connection_t on_geometry_changed = [&](wf::signal_data_t *) {
        on_geometry_changed_impl();
    };

    /// Handle title changes.
    wf::signal_connection_t on_title_changed = [&](wf::signal_data_t *) {
        emit_title_changed();
    };

    /// Destroys the view node and the custom data attached to the view.
    void on_unmapped_impl();

    /// Handle geometry changes.
    void on_geometry_changed_impl();

    /// Temporarily disable handling geometry-changed events if non-zero.
    ///
    /// We temporarily disable handling the geo change event to not react to our
    /// own set_geometry_calls.
    std::uint32_t disable_on_geometry_changed = 0;

    /// Disable reacting to view geometry_changed events.
    void push_disable_on_geometry_changed() { disable_on_geometry_changed++; }

    /// Restore to previous value of disable_on_geometry_changed.
    void pop_disable_on_geometry_changed() {
        assert(disable_on_geometry_changed != 0);
        disable_on_geometry_changed--;
    }

    /// Whether the node is fullscreened.
    bool fullscreen = false;

  public:
    /// The wayfire view corresponding to this node.
    wayfire_view view;

    /// The geo enforcer transformer attached to the view.
    nonstd::observer_ptr<ViewGeoEnforcer> geo_enforcer;

    ViewNode(wayfire_view view);

    ~ViewNode() override;

    /// Try to upgrade this node to a split node.
    ///
    /// A view node is only upgradable to a split if a split preference is set.
    /// When upgraded, the node swaps itself in its parent for the created split
    /// node and adds itself to this split node. Finally, the split preference
    /// is cleared.
    SplitNodeRef try_upgrade();

    /// Get the prefered_split_type of this view node.
    std::optional<SplitType> get_prefered_split_type();

    /// Set the prefered_split_type of this view node.
    void set_prefered_split_type(std::optional<SplitType> split_type);

    /// Get whether the node is fullscreened.
    bool is_fullscreen() { return fullscreen; }

    /// Set whether the node is fullscreened.
    void set_fullscreen(bool f) { fullscreen = f; }

    // == INode impl ==

    void on_initialized() override;
    std::string get_title() override;
    void set_geometry(wf::geometry_t geo) override;
    void set_floating(bool fl) override;
    void set_sublayer(nonstd::observer_ptr<wf::sublayer_t> sublayer) override;
    void bring_to_front() override;
    void on_set_active() override;
    NodeParent get_or_upgrade_to_parent_node() override;
    void for_each_node(const std::function<void(Node)> &f) override;

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

/// Get the ViewNode corresponding to the wayfire view.
///
/// \return The ViewNode of the view or nullptr.
inline ViewNodeRef get_view_node(wayfire_view view) {
    if (auto vdata = view->get_data<ViewData>())
        return vdata->node;
    return nullptr;
}

/// Get the signaled view node.
///
/// \return The ViewNode of the signaled view or nullptr.
inline ViewNodeRef get_signaled_view_node(wf::signal_data_t *data) {
    if (auto ndata = dynamic_cast<ViewNodeSignalData *>(data))
        return ndata->node;
    return get_view_node(wf::get_signaled_view(data));
}

inline SplitNodeRef get_signaled_split_node(wf::signal_data_t *data) {
    auto ndata = dynamic_cast<SplitNodeSignalData *>(data);
    assert(ndata);
    return ndata->node;
}

/// A child of a split node.
///
/// We try to use the size attribute of the children as much as possible in
/// order to make window resizes more stable since using ratios in a continuos
/// resize motion is jumpy as the double gets rounded to pixel amounts.
struct SplitChild {
    std::uint32_t size; ///< The size of the child.
    double ratio;       ///< The size ratio of child.
    OwnedNode node;     ///< A direct child node of the split.
};

using SplitChildIter = std::vector<SplitChild>::iterator;

/// A split node containing children.
class SplitNode final : public INode, public INodeParent {
  private:
    SplitType split_type;             ///< The split type of this node.
    std::uint32_t active_child = 0;   ///< Index of last active child.
    std::vector<SplitChild> children; ///< The direct children nodes.

    /// Find a direct child of this parent node.
    SplitChildIter find_child(Node node);

    /// Set the children ratios to represent the ratios of the sizes with
    /// respect to the total size.
    void sync_ratios_to_sizes();

    /// Set the children sizes to their ratios * total size where total size is
    /// the size of the SplitNode itself.
    void sync_sizes_to_ratios();

    /// Move a direct child outside of this parent in the given direction.
    ///
    /// This either moves the node into an adjacent parent node or at the
    /// back/front of an (in)direct parent.
    bool move_child_outside(SplitChildIter child, Direction dir);

    /// Walk up the tree to find the first split node parent that is (not)
    /// horizontal.
    SplitNodeRef find_parent_split(bool horiz);

    /// Try to move the edge at the back or front of a child by the given amount
    /// of pixels.
    ///
    /// \return the delta actually applied on the edge.
    std::int32_t try_move_edge(SplitChildIter child, std::int32_t delta,
                               bool front, bool use_preferred_sizes = false);

    /// Try to move the edge at the front of a child by the given amount
    /// of pixels.
    ///
    /// \return the delta actually applied on the edge.
    inline std::int32_t try_move_front_edge(SplitChildIter child,
                                            std::int32_t delta,
                                            bool use_preferred_sizes = false);

    /// Try to move the edge at the back of a child by the given amount
    /// of pixels.
    ///
    /// \return the delta actually applied on the edge.
    inline std::int32_t try_move_back_edge(SplitChildIter child,
                                           std::int32_t delta,
                                           bool use_preferred_sizes = false);

  public:
    /// The last orientation of the split. (From last time it was a split and
    /// not a stack)
    bool was_vsplit = true;

    SplitNode(wf::geometry_t geo, SplitType split_type = SplitType::VSPLIT)
        : split_type(split_type) {
        geometry = geo;
        floating_geometry = geo;
    }

    ~SplitNode() override;

    /// Return whether this split contains no children.
    [[nodiscard]] bool empty() const { return children.empty(); }

    /// Return the amount of children nodes under this split node.
    [[nodiscard]] std::size_t get_children_count() const {
        return children.size();
    }

    /// Get a child of this split by index.
    [[nodiscard]] Node child_at(std::size_t i) const noexcept {
        return children.at(i).node.get();
    }

    /// Return whether this is a v/h-split.
    bool is_split() {
        return split_type == SplitType::VSPLIT ||
               split_type == SplitType::HSPLIT;
    };

    /// Return whether this is a stack/tabbed layout.
    bool is_stack() {
        return split_type == SplitType::STACKED ||
               split_type == SplitType::TABBED;
    };

    /// Insert a direct child at the given position in children.
    void insert_child_at(SplitChildIter at, OwnedNode node);

    /// Insert a direct child at the front of children.
    void insert_child_front(OwnedNode node);

    /// Insert a direct child at the back of children.
    void insert_child_back(OwnedNode node);

    /// Insert a direct child just before another direct child.
    void insert_child_front_of(Node of, OwnedNode node);

    /// Insert a direct child just after another direct child.
    void insert_child_back_of(Node of, OwnedNode node);

    /// Remove a direct child from the given position in children.
    OwnedNode remove_child_at(SplitChildIter child);

    /// Get the split type of this node.
    SplitType get_split_type() { return split_type; }

    /// Set the split type of this node.
    void set_split_type(SplitType st);

    /// Try to downgrade this node to its only child node.
    ///
    /// A split node is only downgradable if it contains exactly one direct
    /// child. When downgraded, the node swaps itself in its parent for the only
    /// child node. Finally, if the only child is a view node, the split
    /// preference of it is set to the split type that this split was.
    Node try_downgrade();

    // == INodeParent impl ==

    Node get_adjacent(Node node, Direction dir) override;
    bool move_child(Node node, Direction dir) override;
    wf::dimensions_t try_resize_child(Node child, wf::dimensions_t ndims,
                                      std::uint32_t edges) override;
    Node get_last_active_node() override;
    void insert_child(OwnedNode node) override;
    OwnedNode remove_child(Node node) override;
    OwnedNode swap_child(Node node, OwnedNode other) override;
    void swap_children(Node a, Node b) override;
    void set_active_child(Node node) override;
    Node get_active_child() const override;
    void notify_child_title_changed(Node child) override;

    // == INode impl ==

    void on_initialized() override;
    std::string get_title() override;
    void set_geometry(wf::geometry_t geo) override;
    void begin_resize() override;
    void end_resize() override;
    void set_sublayer(nonstd::observer_ptr<wf::sublayer_t> sublayer) override;
    void bring_to_front() override;
    void set_ws(WorkspaceRef ws) override;
    NodeParent get_or_upgrade_to_parent_node() override;
    void for_each_node(const std::function<void(Node)> &f) override;

    // == IDisplay impl ==

    std::ostream &to_stream(std::ostream &os) const override {
        os << "split-node-" << node_id;
        return os;
    }
};

/// A single workspace managing a tiled tree and floating nodes.
class Workspace final : public INodeParent {
  private:
    /// The root node of a workspace layer.
    template <typename N> struct WorkspaceRoot {
        /// The owned root node.
        std::unique_ptr<N> node;

        /// The sublayer which holds all nodes in under this root.
        nonstd::observer_ptr<wf::sublayer_t> sublayer;
    };

  public:
    /// The workarea of this ws.
    ///
    /// The workarea is the output size minus space reserves for panels and
    /// such.
    wf::geometry_t workarea;

    /// The position of this ws on the ws grid.
    wf::point_t wsid;

    /// The tiled tree that fills this workspace.
    WorkspaceRoot<SplitNode> tiled_root;

    /// The sublayer which holds all floating nodes in this workspace.
    nonstd::observer_ptr<wf::sublayer_t> floating_sublayer;

    /// The floating nodes that are manages by this ws.
    ///
    /// All floating nodes are direct children of their workspace.
    std::vector<WorkspaceRoot<INode>> floating_nodes;

    /// The Swayfire plugin that owns this workspace.
    nonstd::observer_ptr<Swayfire> plugin;

    /// The wayfire output that this workspace is on.
    OutputRef output;

  private:
    using FloatingNodeIter = decltype(Workspace::floating_nodes)::iterator;

    /// Reference to the node currently active in this ws.
    Node active_node = nullptr;

    /// The last active floating node index.
    std::uint32_t active_floating = 0;

    /// Find a floating child of this ws.
    FloatingNodeIter find_floating(Node node);

    /// Handle workarea changes.
    wf::signal_connection_t on_workarea_changed = [&](wf::signal_data_t *data) {
        auto wcdata = static_cast<wf::workarea_changed_signal *>(data);
        set_workarea(wcdata->new_workarea);
    };

    /// Reset the active node to the next valid node in the ws
    void reset_active_node();

  public:
    Workspace(wf::point_t wsid, wf::geometry_t geo,
              nonstd::observer_ptr<Swayfire> swayfire);

    Workspace(const Workspace &) = delete;
    Workspace const &operator=(const Workspace &) = delete;

    ~Workspace() override;

    /// Set the currently active node in this ws.
    ///
    /// Prefer calling node->set_active().
    void set_active_node(Node node);

    /// Get the currently active node in this ws.
    Node get_active_node();

    /// Set the workarea of the workspace.
    void set_workarea(wf::geometry_t geo);

    /// Get the workarea of the workspace.
    wf::geometry_t get_workarea() { return workarea; }

    /// Get the sublayer of the direct child of this workspace.
    nonstd::observer_ptr<wf::sublayer_t> get_child_sublayer(Node child);

    // == Floating ==

    /// Insert a floating node into this workspace.
    void insert_floating_node(OwnedNode node);

    /// Remove a floating node from this workspace.
    ///
    /// Optionally reset the active node in this ws to the next valid candidate.
    /// Set reset_active=false to avoid unfocusing the node.
    OwnedNode remove_floating_node(Node node, bool reset_active = true);

    /// Swap a floating node in this workspace for another node.
    OwnedNode swap_floating_node(Node node, OwnedNode other);

    /// Get the last active floating node in this ws.
    Node get_active_floating_node();

    // == Tiled ==

    /// Insert a tiled node into this ws.
    void insert_tiled_node(OwnedNode node);

    /// Remove a tiled node from this ws.
    ///
    /// Optionally reset the active node in this ws to the next valid candidate.
    /// Set reset_active=false to avoid unfocusing the node.
    OwnedNode remove_tiled_node(Node node, bool reset_active = true);

    /// Swap the root tiled split node with another.
    std::unique_ptr<SplitNode>
    swap_tiled_root(std::unique_ptr<SplitNode> other);

    // == Both ==

    /// Remove a node from this ws.
    ///
    /// Optionally reset the active node in this ws to the next valid candidate.
    ///
    /// A lot of the time remove_node() is called as an intermediary step where
    /// the node will be moved back into the workspace in the same action.
    /// Set reset_active=false to avoid unfocusing the node.
    OwnedNode remove_node(Node node, bool reset_active = true);

    /// Try to (un)tile a node in this workspace.
    void tile_request(Node node, bool tile);

    /// Apply function to all nodes in this workspace.
    void for_each_node(const std::function<void(Node)> &f);

    // == INodeParent impl ==

    Node get_adjacent(Node node, Direction dir) override;
    bool move_child(Node node, Direction dir) override;
    wf::dimensions_t try_resize_child(Node child, wf::dimensions_t ndims,
                                      std::uint32_t edges) override;
    Node get_last_active_node() override;
    void insert_child(OwnedNode node) override;
    OwnedNode remove_child(Node node) override;
    OwnedNode swap_child(Node node, OwnedNode other) override;
    void swap_children(Node a, Node b) override;
    void set_active_child(Node node) override;
    Node get_active_child() const override;

    // == IDisplay impl ==

    std::ostream &to_stream(std::ostream &os) const override {
        os << "workspace-" << wsid;
        return os;
    }
};

/// Grid of all the workspaces on an output.
class Workspaces {
    friend Swayfire;

    /// Workspace tree roots: workspaces[x][y].
    std::vector<std::vector<std::unique_ptr<Workspace>>> workspaces;

  public:
    /// Update the dimensions of the workspace grid.
    void update_dims(wf::dimensions_t ndims, wf::geometry_t geo,
                     nonstd::observer_ptr<Swayfire> plugin);

    /// Get the workspace at the given coordinate in the grid.
    WorkspaceRef get(wf::point_t ws);

    /// Iterate through all workspaces in the grid.
    void for_each(const std::function<void(WorkspaceRef)> &fun);
};

/// Custom wayfire workspace implementation.
class SwayfireWorkspaceImpl final : public wf::workspace_implementation_t {
  public:
    bool view_movable(wayfire_view view) override {
        if (auto node = get_view_node(view))
            return node->get_floating();

        return false;
    }

    bool view_resizable(wayfire_view view) override {
        if (auto node = get_view_node(view))
            return node->get_floating();

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

class Swayfire final : public wf::plugin_interface_t {
  public:
    /// The workspaces manages by swayfire.
    Workspaces workspaces;

  private:
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

    /// Initialize gesture grab interfaces and activators.
    void init_grab_interface();

    /// Destroy gesture grab interfaces and activators.
    void fini_grab_interface();

    /// If necessary, move the view-node to the correct workspace.
    void correct_view_workspace(ViewNodeRef node, wf::point_t correct_ws);

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
    DECL_KEY(set_tabbed);
    DECL_KEY(set_stacked);

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

    /// Handle views being focused.
    wf::signal_connection_t on_view_focused = [&](wf::signal_data_t *data) {
        if (const auto node = get_signaled_view_node(data)) {
            // A potential side-effect of removing a view in a
            // "A.insert_node(B.remove_node())" operation is that in between the
            // two operations, some node is destroyed (e.g. a split downgrade)
            // and this could cause the output to be refocused when the node's
            // subsurfaces are close()'d.
            // TODO: see if the upcoming wayfire transactions api can avoid this
            // problem by making node "A.insert_node(B.remove_node())" more
            // atomic.
            if (!node->parent)
                return;

            node->set_active();
        }
    };

    /// Handle view fullscreen requests
    wf::signal_connection_t on_view_fullscreen_request =
        [&](wf::signal_data_t *data) {
            auto fr_data =
                static_cast<wf::view_fullscreen_request_signal *>(data);

            if (const auto node = get_view_node(fr_data->view)) {
                assert(!fr_data->carried_out);
                fr_data->carried_out = true;

                if (fr_data->state == node->is_fullscreen())
                    return;

                // FIXME: ignoring target workspace

                if (fr_data->state) {
                    node->geo_enforcer->ref_disable();
                    node->ref_pure_set_geo();
                    node->view->set_geometry(fr_data->desired_size);
                } else {
                    node->geo_enforcer->unref_disable();
                    node->unref_pure_set_geo();

                    if (node->get_floating())
                        node->set_geometry(fr_data->desired_size);
                    else
                        node->refresh_geometry();
                }
                node->set_fullscreen(fr_data->state);

                return;
            }
        };

    /// Handle view tile requests
    wf::signal_connection_t on_view_tile_request =
        [&](wf::signal_data_t *data) {
            auto tr_data = static_cast<wf::view_tile_request_signal *>(data);

            if (const auto node = get_view_node(tr_data->view)) {
                assert(!tr_data->carried_out);
                tr_data->carried_out = true;
                // Following the example of wayfire's simple_tile, we ignore
                // tile requests from wayfire since we manually handle what
                // views tile when.
                return;
            }
        };

    /// Handle new created views.
    wf::signal_connection_t on_view_attached = [&](wf::signal_data_t *data) {
        auto view = wf::get_signaled_view(data);

        if (view->role != wf::VIEW_ROLE_TOPLEVEL || view->parent)
            return;

        auto ws = workspaces.get(nonwf::get_view_workspace(view));

        LOGD("attaching node in ", ws, ", ", view->to_string(), " : ",
             view->get_title());

        ws->insert_tiled_node(init_view_node(view));
    };

    /// Handle (un)minimized views.
    wf::signal_connection_t on_view_minimized = [&](wf::signal_data_t *data) {
        auto minimizing =
            dynamic_cast<wf::view_minimize_request_signal *>(data)->state;
        if (minimizing) {
            if (auto vnode = get_signaled_view_node(data)) {
                (void)vnode->get_ws()->remove_node(vnode);
                // view node dies here.
            }
        } else {
            on_view_attached.emit(data);
        }
    };

    /// Handle views changing viewport.
    wf::signal_connection_t on_view_change_viewport =
        [&](wf::signal_data_t *data_) {
            const auto data =
                dynamic_cast<wf::view_change_viewport_signal *>(data_);
            if (const auto view_node = get_view_node(data->view)) {
                if (const auto floating = view_node->find_floating_parent()) {
                    // NOTE: We don't do anything if we are part of a floating
                    // tree since it is actually possible for a member of a
                    // floating tree to overflow into a different workspace.
                    (void)0;
                } else {
                    // With a normal tiled node though, it should be impossible
                    // for it to not be in the same workspace as the rest of the
                    // tree. So we correct it here.

                    auto from = view_node->get_ws();
                    if (data->to != from->wsid)
                        workspaces.get(data->to)->insert_tiled_node(
                            from->remove_tiled_node(view_node));
                }
            }
        };

    /// Handle active workspace changing.
    wf::signal_connection_t on_workspace_changed =
        [&](wf::signal_data_t *data_) {
            const auto data =
                dynamic_cast<wf::workspace_changed_signal *>(data_);
            const auto views = output->workspace->get_views_on_workspace(
                data->new_viewport, wf::LAYER_WORKSPACE);

            for (const auto &view : views) {
                if (const auto view_node = get_view_node(view)) {
                    correct_view_workspace(
                        view_node, output->workspace->get_current_workspace());
                }
            }
        };

  public:
    WorkspaceRef get_current_workspace();
    WorkspaceRef get_view_workspace(wayfire_view view,
                                    bool with_transform = false);

    // == Impl wf::plugin_interface_t ==

    void init() override;
    void fini() override;
    ~Swayfire() override;
};

#endif // ifndef SWAYFIRE_CORE_HPP
