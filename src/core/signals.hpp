#ifndef SWAYFIRE_SIGNALS_HPP
#define SWAYFIRE_SIGNALS_HPP

#include <wayfire/geometry.hpp>
#include <wayfire/nonstd/observer_ptr.h>
#include <wayfire/object.hpp>

class INode;
class SplitNode;
class ViewNode;

using Node = nonstd::observer_ptr<INode>;
using SplitNodeRef = nonstd::observer_ptr<SplitNode>;
using ViewNodeRef = nonstd::observer_ptr<ViewNode>;

// ========================================================================== //
// == Swayfire Lifecycle ==

/// NAME: swf-init
/// ON: output
/// WHEN: After swayfire is initialized.

/// NAME: swf-fini
/// ON: output
/// WHEN: Before swayfire is finalized.

// ========================================================================== //
// == Output Signals ==

/// NAME: swf-active-node-changed
/// ON: output
/// WHEN: When a workspace on the output's active node changes.
struct ActiveNodeChangedSignalData : wf::signal_data_t {
    Node old_node, new_node;
};

// ========================================================================== //
// == Node Lifecycle ==

/// NAME: swf-view-node-attached
/// ON: output
/// WHEN: After the view node is initialized.

/// NAME: detached
/// ON: ViewNode, output(swf-view-node-)
/// WHEN: When the view node is destroyed.

/// NAME: swf-split-node-attached
/// ON: output
/// WHEN: After the split node is initialized.

// ========================================================================== //
// == Node Signals ==

/// NAME: geometry-changed
/// ON: INode
/// WHEN: When the node's geometry is set.

struct GeometryChangedSignalData : wf::signal_data_t {
    wf::geometry_t old_geo, new_geo;
};

// ========================================================================== //
// == View Node Signals ==

/// NAME: prefered-split-type-changed
/// ON: ViewNode
/// WHEN: When the view node's prefered_split_type changes.

// ========================================================================== //
// == Split Node Signals ==

/// NAME: child-inserted
/// ON: SplitNode
/// WHEN: When a new child is inserted into the node.

/// NAME: child-removed
/// ON: SplitNode
/// WHEN: When a child is removed from the node.

/// NAME: child-swapped
/// ON: SplitNode
/// WHEN: When a child of the node is swapped for another node.

struct ChildSwappedSignalData : wf::signal_data_t {
    Node old_node; ///< The swapped-out node.
    Node new_node; ///< The swapped-in node.
};

/// NAME: children-swapped
/// ON: SplitNode
/// WHEN: When two of the node's children are swapped.

/// NAME: split-type-changed
/// ON: SplitNode
/// WHEN: When the split type of the node changes.

/// Data passed on view-node signals emitted from swayfire
struct ViewNodeSignalData : wf::signal_data_t {
    /// The node that triggered the signal
    ViewNodeRef node;
};

/// Data passed on split-node signals emitted from swayfire
struct SplitNodeSignalData : wf::signal_data_t {
    /// The node that triggered the signal
    SplitNodeRef node;
};

/// Data passed on node signals emitted from swayfire
struct NodeSignalData : wf::signal_data_t {
    /// The node that triggered the signal
    Node node;
};

#endif // ifndef SWAYFIRE_SIGNALS_HPP
