#include "swayfire.hpp"
#include "grab.hpp"
#include <algorithm>
#include <functional>
#include <iterator>
#include <memory>
#include <optional>
#include <utility>
#include <wayfire/config/types.hpp>
#include <wayfire/core.hpp>
#include <wayfire/geometry.hpp>
#include <wayfire/util/log.hpp>
#include <wlr/util/edges.h>

// nonwf

// TODO: use wf::get_view_main_workspace once we upgrade to wayfire > 0.7
wf::point_t nonwf::get_view_workspace(wayfire_view view, OutputRef output) {
    auto og = output->get_screen_size();

    auto wm = view->transform_region(view->get_wm_geometry());
    auto x = (int)std::floor((wm.x + wm.width / 2.0) / og.width);
    auto y = (int)std::floor((wm.y + wm.height / 2.0) / og.height);

    auto dims = output->workspace->get_workspace_grid_size();
    auto curr = output->workspace->get_current_workspace();

    // convert relative coords to nearest absolute wsid coords
    x = std::clamp(x + curr.x, 0, dims.width - 1);
    y = std::clamp(y + curr.y, 0, dims.height - 1);

    return {x, y};
}

wf::geometry_t nonwf::local_to_relative_geometry(wf::geometry_t geo,
                                                 wf::point_t from_wsid,
                                                 wf::point_t to_wsid,
                                                 OutputRef output) {

    auto og = output->get_screen_size();

    geo.x += og.width * (from_wsid.x - to_wsid.x);
    geo.y += og.height * (from_wsid.y - to_wsid.y);

    return geo;
}

wf::point_t nonwf::geometry_center(wf::geometry_t geo) {
    return {
        (int)std::floor((float)(geo.x + geo.width) / 2.0f),
        (int)std::floor((float)(geo.y + geo.height) / 2.0f),
    };
}

// INodeParent

SplitNodeRef INodeParent::as_split_node() {
    return dynamic_cast<SplitNode *>(this);
}

// INode

SplitNodeRef INode::as_split_node() { return dynamic_cast<SplitNode *>(this); }

ViewNodeRef INode::as_view_node() { return dynamic_cast<ViewNode *>(this); }

void INode::try_resize(wf::dimensions_t ndims, uint32_t edges) {
    if (get_floating()) {
        auto ngeo = get_geometry();

        auto hori_locked = (edges & (WLR_EDGE_LEFT | WLR_EDGE_RIGHT)) == 0;
        auto vert_locked = (edges & (WLR_EDGE_TOP | WLR_EDGE_BOTTOM)) == 0;

        if (!hori_locked) {
            if (edges & WLR_EDGE_LEFT) {
                int dw = ndims.width - ngeo.width;
                ngeo.x -= dw;
            }
            ngeo.width = ndims.width;
        }

        if (!vert_locked) {
            if (edges & WLR_EDGE_TOP) {
                int dh = ndims.height - ngeo.height;
                ngeo.y -= dh;
            }
            ngeo.height = ndims.height;
        }

        set_geometry(ngeo);
    }
}

void INode::set_active() {
    parent->set_active_child(this);
    ws->set_active_node(this);
}

Node INode::find_floating_parent() {
    if (get_floating())
        return this;

    if (auto p = parent->as_split_node())
        return p->find_floating_parent();

    return nullptr;
}

// ViewGeoEnforcer

ViewGeoEnforcer::ViewGeoEnforcer(ViewNodeRef node)
    : wf::view_2D(node->view), view_node(node) {
    view->connect_signal("geometry-changed", &on_geometry_changed);
}

ViewGeoEnforcer::~ViewGeoEnforcer() {
    view->disconnect_signal(&on_geometry_changed);
}

void ViewGeoEnforcer::update_transformer() {
    auto curr = view->get_wm_geometry();

    if (curr.width <= 0 && curr.height <= 0)
        return;

    auto geo = view_node->get_geometry();

    auto output = view_node->ws->output;
    auto wsid = view_node->ws->wsid;
    auto curr_wsid = output->workspace->get_current_workspace();
    if (wsid != curr_wsid)
        geo = nonwf::local_to_relative_geometry(geo, wsid, curr_wsid, output);

    if (curr == geo) {
        scale_x = 1;
        scale_y = 1;
        translation_x = 0;
        translation_y = 0;
        view->damage();
        return;
    }

    scale_x = (float)geo.width / (float)curr.width;
    scale_y = (float)geo.height / (float)curr.height;

    translation_x = (float)geo.x - (float)curr.x +
                    ((float)geo.width - (float)curr.width) / 2.0f;
    translation_y = (float)geo.y - (float)curr.y +
                    ((float)geo.height - (float)curr.height) / 2.0f;

    view->damage();
}

// ViewNode

ViewNode::ViewNode(wayfire_view view) : view(view) {
    auto ge = std::make_unique<ViewGeoEnforcer>(this);
    geo_enforcer = ge.get();
    view->add_transformer(std::move(ge));

    geometry = view->get_wm_geometry();
    floating_geometry = geometry;

    view->connect_signal("mapped", &on_mapped);
    view->connect_signal("unmapped", &on_unmapped);
    view->get_output()->connect_signal("view-focused", &on_focused);
}

ViewNode::~ViewNode() {
    LOGD("Destroying ", this);

    view->get_output()->disconnect_signal(&on_focused);
    view->disconnect_signal(&on_unmapped);
    view->disconnect_signal(&on_mapped);

    view->pop_transformer(geo_enforcer);

    view->erase_data<ViewData>();
}

void ViewNode::on_unmapped_impl() {
    // ws might get unset on remove_child so we must save it.
    auto ws = this->ws;
    auto old_parent = parent;

    parent->remove_child(this);
    if (auto split = old_parent->as_split_node())
        split->try_downgrade();

    ws->node_removed(this);

    // view node dies here.
}

void ViewNode::set_floating(bool fl) {
    if (!floating && fl)
        set_geometry(floating_geometry);

    if (floating && !fl)
        floating_geometry = get_geometry();

    floating = fl;
    view->set_tiled(fl ? 0 : wf::TILED_EDGES_ALL);
}

void ViewNode::set_geometry(wf::geometry_t geo) {
    geometry = geo;

    auto curr_wsid = ws->output->workspace->get_current_workspace();
    if (ws->wsid != curr_wsid)
        geo = nonwf::local_to_relative_geometry(geo, ws->wsid, curr_wsid,
                                                ws->output);

    view->set_geometry(geo);
    geo_enforcer->update_transformer();
}

SplitNodeRef ViewNode::try_upgrade() {
    if (prefered_split_type) {
        auto new_parent = std::make_unique<SplitNode>(get_geometry());
        auto new_parent_ref = new_parent.get();
        new_parent->split_type = *prefered_split_type;
        auto owned_self = parent->swap_child(this, std::move(new_parent));
        owned_self->set_floating(false);
        new_parent_ref->insert_child_back(std::move(owned_self));

        prefered_split_type = {};
        return new_parent_ref;
    }
    return nullptr;
}

void ViewNode::set_active() {
    INode::set_active();

    if (!view->activated)
        view->focus_request();
}

NodeParent ViewNode::get_or_upgrade_to_parent_node() {
    if (auto split = try_upgrade())
        return split;
    else
        return parent;
}

// SplitNode

void SplitNode::insert_child_at(SplitChildIter at, OwnedNode node) {
    node->parent = this;
    node->set_floating(false);
    node->set_ws(get_ws());

    float shrink_ratio = (float)children.size() / (float)(children.size() + 1);
    float total_ratio = 0;

    for (auto &c : children) {
        c.ratio *= shrink_ratio;
        total_ratio += c.ratio;
    }

    SplitChild nchild;
    nchild.node = std::move(node);
    nchild.ratio = 1.0f - total_ratio;

    children.insert(at, std::move(nchild));
    refresh_geometry();
}

void SplitNode::insert_child_front(OwnedNode node) {
    insert_child_at(children.begin(), std::move(node));
}

void SplitNode::insert_child_back(OwnedNode node) {
    insert_child_at(children.end(), std::move(node));
}

void SplitNode::insert_child(OwnedNode node) {
    insert_child_back(std::move(node));
};

void SplitNode::insert_child_front_of(Node of, OwnedNode node) {
    auto child = find_child(of);
    if (child == children.end())
        LOGE("Node ", of, " not found in split node: ", this);

    insert_child_at(child, std::move(node));
}

void SplitNode::insert_child_back_of(Node of, OwnedNode node) {
    auto child = find_child(of);
    if (child == children.end())
        LOGE("Node ", of, " not found in split node: ", this);

    insert_child_at(child + 1, std::move(node));
}

SplitChildIter SplitNode::find_child(Node node) {
    return std::find_if(children.begin(), children.end(),
                        [&](auto &c) { return c.node.get() == node.get(); });
}

OwnedNode SplitNode::remove_child(Node node) {
    auto child = find_child(node);
    if (child == children.end())
        LOGE("Node ", node, " not found in split node: ", this);

    return remove_child_at(child);
}

OwnedNode SplitNode::remove_child_at(SplitChildIter child) {
    auto owned_node = std::move(child->node);
    children.erase(child);

    if (children.empty()) {
        active_child = 0;
    } else {
        active_child = std::clamp(active_child, (uint32_t)0,
                                  (uint32_t)(children.size() - 1));
    }

    if (!children.empty()) {
        float grow_ratio =
            (float)(children.size() + 1) / (float)(children.size());
        float total_ratio = 0;
        for (auto i = children.begin(); i != children.end() - 1; i++) {
            i->ratio *= grow_ratio;
            total_ratio += i->ratio;
        }
        children.back().ratio = 1.0f - total_ratio;
    }

    refresh_geometry();

    owned_node->parent = nullptr;

    return owned_node;
}

void SplitNode::set_active_child(Node node) {
    auto child = find_child(node);
    if (child == children.end()) {
        LOGE("Node ", node, " not found in split node: ", this);
        return;
    }

    active_child = std::distance(children.begin(), child);

    parent->set_active_child(this);
}

void SplitNode::toggle_split_direction() {
    LOGD("Toggling split dir: ", parent);

    split_type = (split_type == SplitType::HSPLIT) ? SplitType::VSPLIT
                                                   : SplitType::HSPLIT;

    refresh_geometry();
}

Node SplitNode::try_downgrade() {
    if (children.size() == 1) {
        // Can only swap tiled_root of workspace with a split node.
        if (get_ws()->tiled_root.get() == this &&
            !children.front().node->as_split_node())
            return nullptr;

        auto only_child = remove_child_at(children.begin() + active_child);
        auto only_child_ref = only_child.get();

        if (auto vnode = only_child->as_view_node())
            vnode->prefered_split_type = split_type;

        bool was_active = false;
        if (auto active = get_ws()->get_active_node())
            was_active = active.get() == this;

        parent->swap_child(this, std::move(only_child));

        if (was_active)
            only_child_ref->set_active();

        return only_child_ref;
    }
    return nullptr;
}

NodeParent SplitNode::get_or_upgrade_to_parent_node() { return this; }

OwnedNode SplitNode::swap_child(Node node, OwnedNode other) {
    auto child = find_child(node);
    if (child == children.end())
        LOGE("Node ", node, " not found in split node: ", this);

    other->parent = this;
    other->set_floating(false);
    other->set_ws(get_ws());
    other->set_geometry(child->node->get_geometry());

    child->node.swap(other);

    return other;
}

Node SplitNode::get_last_active_node() {
    if (children.empty())
        return nullptr;

    auto &child = children.at(active_child);
    if (auto split = child.node->as_split_node())
        if (auto la = split->get_last_active_node())
            return la;

    return child.node.get();
}

Node SplitNode::get_adjacent(Node node, Direction dir) {
    auto child = find_child(node);
    if (child == children.end())
        LOGE("Node ", node, " not found in split node: ", this);

    switch (split_type) {
    case SplitType::VSPLIT:
    case SplitType::TABBED: {
        switch (dir) {
#define PREV_NODE                                                              \
    child == children.begin() ? parent->get_adjacent(this, dir)                \
                              : child[-1].node.get()
#define NEXT_NODE                                                              \
    child == (children.end() - 1) ? parent->get_adjacent(this, dir)            \
                                  : child[1].node.get()
        case Direction::LEFT:
            return PREV_NODE;
        case Direction::RIGHT:
            return NEXT_NODE;
        case Direction::UP:
        case Direction::DOWN:
            return parent->get_adjacent(this, dir);
        }
    }
    case SplitType::HSPLIT:
    case SplitType::STACKED: {
        switch (dir) {
        case Direction::LEFT:
        case Direction::RIGHT:
            return parent->get_adjacent(this, dir);
        case Direction::UP:
            return PREV_NODE;
        case Direction::DOWN:
            return NEXT_NODE;
#undef NEXT_NODE
#undef PREV_NODE
        }
        break;
    }
    }
}

SplitNodeRef SplitNode::find_parent_split(bool horiz) {
    auto p = parent->as_split_node();
    bool horizontal = false;
    bool vertical = false;
    while (p) {
        switch (p->split_type) {
        case SplitType::VSPLIT:
        case SplitType::TABBED:
            horizontal = true;
            break;
        case SplitType::HSPLIT:
        case SplitType::STACKED:
            vertical = true;
            break;
        }
        if (horiz && horizontal)
            break;
        if (!horiz && vertical)
            break;
        p = p->parent->as_split_node();
    }
    if ((horizontal && horiz) || (vertical && !horiz))
        return p;
    else
        return nullptr;
}

bool SplitNode::move_child_outside(SplitChildIter child, Direction dir) {
    if (auto adj = parent->get_adjacent(this, dir)) {
        if (auto adj_split = adj->parent->as_split_node()) {
            switch (dir) {
            case Direction::LEFT:
            case Direction::UP:
                adj_split->insert_child_back_of(adj, remove_child_at(child));
                break;
            case Direction::RIGHT:
            case Direction::DOWN:
                adj_split->insert_child_front_of(adj, remove_child_at(child));
                break;
            }
            return true;
        }
    } else {
        switch (dir) {
        case Direction::LEFT:
            if (auto p = find_parent_split(true)) {
                p->insert_child_front(remove_child_at(child));
                return true;
            }
            break;
        case Direction::RIGHT:
            if (auto p = find_parent_split(true)) {
                p->insert_child_back(remove_child_at(child));
                return true;
            }
            break;
        case Direction::UP:
            if (auto p = find_parent_split(false)) {
                p->insert_child_front(remove_child_at(child));
                return true;
            }
            break;
        case Direction::DOWN:
            if (auto p = find_parent_split(false)) {
                p->insert_child_back(remove_child_at(child));
                return true;
            }
            break;
        }
    }
    return false;
}

bool SplitNode::move_child(Node node, Direction dir) {
    auto child = find_child(node);
    if (child == children.end())
        LOGE("Node ", node, " not found in split node: ", this);

#define MOVE_BACK()                                                            \
    {                                                                          \
        if (child == children.begin()) {                                       \
            return move_child_outside(child, dir);                             \
        } else {                                                               \
            if (auto adj_split = child[-1].node->as_split_node()) {            \
                adj_split->insert_child_back(remove_child_at(child));          \
                return true;                                                   \
            } else if (auto adj_view = child[-1].node->as_view_node()) {       \
                if (auto adj_split = adj_view->try_upgrade()) {                \
                    adj_split->insert_child_back(remove_child_at(child));      \
                    return true;                                               \
                }                                                              \
            }                                                                  \
            insert_child_front_of(child[-1].node.get(),                        \
                                  remove_child_at(child));                     \
            return true;                                                       \
        }                                                                      \
    }

#define MOVE_FORWARD()                                                         \
    {                                                                          \
        if (child == children.end() - 1) {                                     \
            return move_child_outside(child, dir);                             \
        } else {                                                               \
            if (auto adj_split = child[1].node->as_split_node()) {             \
                adj_split->insert_child_front(remove_child_at(child));         \
                return true;                                                   \
            } else if (auto adj_view = child[1].node->as_view_node()) {        \
                if (auto adj_split = adj_view->try_upgrade()) {                \
                    adj_split->insert_child_front(remove_child_at(child));     \
                    return true;                                               \
                }                                                              \
            }                                                                  \
            insert_child_back_of(child[1].node.get(), remove_child_at(child)); \
            return true;                                                       \
        }                                                                      \
    }

    switch (split_type) {
    case SplitType::VSPLIT:
    case SplitType::TABBED: {
        switch (dir) {
        case Direction::LEFT:
            MOVE_BACK();
            break;
        case Direction::RIGHT:
            MOVE_FORWARD();
            break;
        case Direction::UP:
        case Direction::DOWN:
            return move_child_outside(child, dir);
        }
    }
    case SplitType::HSPLIT:
    case SplitType::STACKED: {
        switch (dir) {
        case Direction::UP:
            MOVE_BACK();
            break;
        case Direction::DOWN:
            MOVE_FORWARD();
            break;
        case Direction::LEFT:
        case Direction::RIGHT:
            return move_child_outside(child, dir);
        }
    }
    }
#undef MOVE_FORWARD
#undef MOVE_BACK
}

void SplitNode::set_floating(bool fl) { floating = fl; }

void SplitNode::set_ws(WorkspaceRef ws) {
    INode::set_ws(ws);

    for (auto &child : children)
        child.node->set_ws(ws);
}

void SplitNode::set_geometry(wf::geometry_t geo) {
    switch (split_type) {
// distribute over dim1 and pos1
#define DISTRIBUTE(dim1, dim2, pos1, pos2)                                     \
    {                                                                          \
        auto total = 0;                                                        \
        unsigned int i = 0;                                                    \
        for (auto &child : children) {                                         \
            auto subgeo = child.node->get_geometry();                          \
                                                                               \
            subgeo.pos1 = geo.pos1 + total;                                    \
            subgeo.pos2 = geo.pos2;                                            \
                                                                               \
            if (i == children.size() - 1) {                                    \
                subgeo.dim1 = geo.dim1 - total;                                \
            } else {                                                           \
                subgeo.dim1 = geo.dim1 * child.ratio;                          \
                total += subgeo.dim1;                                          \
            }                                                                  \
            subgeo.dim2 = geo.dim2;                                            \
                                                                               \
            child.node->set_geometry(subgeo);                                  \
            i++;                                                               \
        }                                                                      \
    }
    case SplitType::VSPLIT:
        DISTRIBUTE(width, height, x, y);
        break;
    case SplitType::HSPLIT:
        DISTRIBUTE(height, width, y, x);
        break;
#undef DISTRIBUTE
    case SplitType::TABBED: {
        for (auto &child : children)
            child.node->set_geometry(geo);
        break;
    }
    case SplitType::STACKED: {
        for (auto &child : children)
            child.node->set_geometry(geo);
        break;
    }
    }
    geometry = geo;
}

// Workspace

Workspace::Workspace(wf::point_t wsid, wf::geometry_t geo, OutputRef output)
    : workarea(geo), wsid(wsid), output(output) {
    (void)swap_tiled_root(std::make_unique<SplitNode>(geo));
    LOGD("ws created with root ", tiled_root->to_string());
    active_node = tiled_root;

    output->connect_signal("workarea-changed", &on_workarea_changed);
}

Workspace::~Workspace() { output->disconnect_signal(&on_workarea_changed); }

void Workspace::set_active_node(Node node) {
    if (!node->get_floating())
        active_tiled_node = node;

    active_node = node;
}

Node Workspace::get_active_node() { return active_node; }

void Workspace::insert_floating_node(OwnedNode node) {
    node->parent = this;
    node->set_floating(true);
    node->set_ws(this);
    floating_nodes.push_back(std::move(node));
}

NodeIter Workspace::find_floating(Node node) {
    return std::find_if(floating_nodes.begin(), floating_nodes.end(),
                        [&](auto &c) { return c.get() == node.get(); });
}

OwnedNode Workspace::remove_floating_node(Node node) {
    auto &fl = floating_nodes;
    auto child = find_floating(node);
    if (child == fl.end()) {
        LOGE("Node not floating in ", this, ": ", node);
        return nullptr;
    }

    auto owned_node = std::move(*child);

    fl.erase(child);

    if (floating_nodes.empty())
        active_floating = 0;
    else
        active_floating = std::clamp(active_floating, (uint32_t)0,
                                     (uint32_t)(floating_nodes.size() - 1));

    return owned_node;
}

OwnedNode Workspace::swap_floating_node(Node node, OwnedNode other) {
    auto &fl = floating_nodes;
    auto child = find_floating(node);
    if (child == fl.end()) {
        LOGE("Node not floating in ", this, ": ", node);
        return nullptr;
    }

    other->parent = this;
    other->set_floating(true);
    other->set_ws(this);
    other->set_geometry((*child)->get_geometry());

    (*child).swap(other);

    return other;
}

Node Workspace::get_active_floating_node() {
    if (floating_nodes.empty())
        return nullptr;

    return floating_nodes.at(active_floating).get();
}

Node Workspace::get_active_tiled_node() { return active_tiled_node; }

OwnedNode Workspace::swap_tiled_root(std::unique_ptr<SplitNode> other) {
    auto ret = std::move(tiled_root);

    tiled_root = std::move(other);
    tiled_root->parent = this;
    tiled_root->set_floating(false);
    tiled_root->set_ws(this);

    return ret;
}

void Workspace::insert_tiled_node(OwnedNode node) {
    auto parent = get_active_node()->get_or_upgrade_to_parent_node();

    parent->insert_child(std::move(node));
}

OwnedNode Workspace::remove_tiled_node(Node node) {
    if (node->get_floating() || node->get_ws().get() != this) {
        LOGE("Node not tiled in ", this, ": ", node);
        return nullptr;
    }

    auto old_parent = node->parent;
    auto owned_node = node->parent->remove_child(node);

    if (active_tiled_node.get() == node.get())
        active_tiled_node = old_parent->get_last_active_node();

    return owned_node;
}

OwnedNode Workspace::remove_node(Node node) {
    return node->get_floating() ? remove_floating_node(node)
                                : remove_tiled_node(node);
}

void Workspace::node_removed(Node node) {
    // The case for a floating node is already covered in remove_floating_node
    // as floating nodes are always direct children of the ws.

    if (node.get() == active_tiled_node.get())
        active_tiled_node = tiled_root.get();

    if (node.get() == active_node.get())
        active_node = active_tiled_node;
}

void Workspace::insert_child(OwnedNode node) {
    tiled_root->insert_child(std::move(node));
}

OwnedNode Workspace::remove_child(Node node) {
    OwnedNode ret;

    if (node->get_floating()) {
        // floating nodes are always direct children of the workspace
        ret = remove_floating_node(node);
    } else if (node.get() == tiled_root.get()) {
        ret = swap_tiled_root(std::make_unique<SplitNode>(workarea));
        if (ret.get() == active_tiled_node.get()) {
            if (auto new_active = tiled_root->get_last_active_node())
                active_tiled_node = new_active;
            else
                active_tiled_node = tiled_root.get();
        }

        if (ret.get() == active_node.get())
            active_node = active_tiled_node;
    } else {
        LOGE("Node is not a direct child of ", this, ": ", node);
        return nullptr;
    }

    if (node.get() == get_active_node().get())
        tiled_root->set_active();

    return ret;
}

OwnedNode Workspace::swap_child(Node node, OwnedNode other) {
    if (node->get_floating()) {
        // floating nodes are always direct children of the workspace
        return swap_floating_node(node, std::move(other));
    } else if (node.get() == tiled_root.get()) {
        auto other_ = other.release();
        if (auto other_split = dynamic_cast<SplitNode *>(other_))
            return swap_tiled_root(std::unique_ptr<SplitNode>(other_split));

        LOGE("Cannot swap non-split node with tiled-root node of ", this);

        delete other_;
        return nullptr;
    } else {
        LOGE("Node is not a direct child of ", this, ": ", node);
        return nullptr;
    }
}

void Workspace::set_active_child(Node node) {
    if (node->get_floating()) {
        auto child = find_floating(node);
        if (child == floating_nodes.end()) {
            LOGE("Node not in ", this, ": ", node);
        }
        active_floating = std::distance(floating_nodes.begin(), child);
    } else {
        active_tiled_node = node;
    }
}

void Workspace::set_workarea(wf::geometry_t geo) {
    workarea = geo;
    tiled_root->set_geometry(geo);

    for (auto &floating : floating_nodes) {
        auto ngeo = floating->get_geometry();

        // Make sure they are still visible in the workarea.

        if (geo.width > MIN_VIEW_SIZE)
            ngeo.x = std::clamp(ngeo.x, MIN_VIEW_SIZE - ngeo.width,
                                geo.width - MIN_VIEW_SIZE);

        if (geo.height > MIN_VIEW_SIZE)
            ngeo.y = std::clamp(ngeo.y, MIN_VIEW_SIZE - ngeo.height,
                                geo.height - MIN_VIEW_SIZE);

        // Refresh their geometry.
        floating->set_geometry(ngeo);
    }
}

void Workspace::toggle_tile_node(Node node) {
    LOGD("toggling tiling for ", node);

    if (node->get_floating()) {
        insert_tiled_node(remove_floating_node(node));
        if (active_node.get() == node.get())
            active_tiled_node = node.get();
    } else {
        insert_floating_node(remove_tiled_node(node));
        if (active_node.get() == node.get())
            active_floating =
                std::distance(floating_nodes.begin(), find_floating(node));
    }
}

Node Workspace::get_last_active_node() { return active_node; }

Node Workspace::get_adjacent(Node node, Direction dir) {
    if (node.get() == tiled_root.get()) {
        LOGE("No node is adjacent to root tiled node : ", node);
        return nullptr;
    } else {
        auto child = find_floating(node);
        if (child == floating_nodes.end()) {
            LOGE("Node not in ", this, ": ", node);
            return nullptr;
        }

        auto &fl = floating_nodes;
        auto center = nonwf::geometry_center(node->get_geometry());
        switch (dir) {
#define CLOSEST(axis, cmp)                                                     \
    {                                                                          \
        auto closest =                                                         \
            std::min_element(fl.begin(), fl.end(), [&](auto &a, auto &b) {     \
                if (a.get() == node.get())                                     \
                    return false;                                              \
                if (b.get() == node.get())                                     \
                    return true;                                               \
                                                                               \
                auto apoint = nonwf::geometry_center(a->get_geometry());       \
                auto bpoint = nonwf::geometry_center(b->get_geometry());       \
                                                                               \
                if (center.axis cmp apoint.axis)                               \
                    return false;                                              \
                if (center.axis cmp bpoint.axis)                               \
                    return true;                                               \
                                                                               \
                return std::abs(center.axis - apoint.axis) <                   \
                       std::abs(center.axis - bpoint.axis);                    \
            });                                                                \
        if (closest == fl.end() || (*closest).get() == node.get() ||           \
            center.axis cmp nonwf::geometry_center((*closest)->get_geometry()) \
                .axis)                                                         \
            return nullptr;                                                    \
        else                                                                   \
            return (*closest).get();                                           \
    }
        case Direction::LEFT:
            CLOSEST(x, <);
        case Direction::RIGHT:
            CLOSEST(x, >);
        case Direction::UP:
            CLOSEST(y, <);
        case Direction::DOWN:
            CLOSEST(y, >);
#undef CLOSEST
        }
    }
}

bool Workspace::move_child(Node node, Direction dir) {
    if (node->get_floating()) {
        auto geo = node->get_geometry();

        switch (dir) {
        case Direction::LEFT:
            geo.x -= FLOATING_MOVE_STEP;
            break;
        case Direction::RIGHT:
            geo.x += FLOATING_MOVE_STEP;
            break;
        case Direction::UP:
            geo.y -= FLOATING_MOVE_STEP;
            break;
        case Direction::DOWN:
            geo.y += FLOATING_MOVE_STEP;
            break;
        }

        node->set_geometry(geo);
        return true;
    }
    return false;
}

// Workspaces

void Workspaces::update_dims(wf::dimensions_t ndims, wf::geometry_t geo,
                             OutputRef output) {

    workspaces.resize(ndims.width);

    auto x = 0;
    for (auto &col : workspaces) {
        if (col.size() == (uint32_t)ndims.height) {
            // noop
        } else if (col.size() < (uint32_t)ndims.height) {
            col.reserve(ndims.height);
            for (int32_t y = col.size(); y < ndims.height; y++) {
                col.push_back(std::unique_ptr<Workspace>(
                    new Workspace({x, y}, geo, output)));
            }
        } else {
            col.erase(col.begin() + ndims.height, col.end());
        }
        x++;
    }
}

WorkspaceRef Workspaces::get(wf::point_t ws) {
    return workspaces.at(ws.x).at(ws.y).get();
}

void Workspaces::for_each(const std::function<void(WorkspaceRef)> &fun) {
    for (auto &col : workspaces)
        for (auto &ws : col)
            fun(ws.get());
}

// Swayfire

WorkspaceRef Swayfire::get_current_workspace() {
    auto wsid = output->workspace->get_current_workspace();
    return workspaces.get(wsid);
}

std::unique_ptr<ViewNode> Swayfire::init_view_node(wayfire_view view) {
    auto node = std::make_unique<ViewNode>(view);
    view->store_data<ViewData>(std::make_unique<ViewData>(node));

    LOGD("New view-node for ", view->to_string(), ": ", node.get());
    return node;
}

void Swayfire::bind_signals() {
    output->connect_signal("view-layer-attached", &on_view_attached);
}

void Swayfire::unbind_signals() {
    output->disconnect_signal(&on_view_attached);
}

void Swayfire::init() {
    LOGD("==== init ====");
    output->workspace->set_workspace_implementation(
        std::make_unique<SwayfireWorkspaceImpl>(), true);

    auto grid_dims = output->workspace->get_workspace_grid_size();

    workspaces.update_dims(grid_dims, output->workspace->get_workarea(),
                           output);

    auto views = output->workspace->get_views_in_layer(wf::ALL_LAYERS);

    for (auto view : views) {
        if (view->role == wf::VIEW_ROLE_TOPLEVEL) {
            auto ws = workspaces.get(nonwf::get_view_workspace(view, output));
            ws->insert_tiled_node(init_view_node(view));
        }
    }

    if (auto active_view = output->get_active_view()) {
        if (auto vdata = active_view->get_data<ViewData>()) {
            vdata->node->set_active();
        }
    }

    init_grab_interface();

    bind_signals();
    bind_keys();
}

void Swayfire::fini() {
    LOGD("==== fini ====");

    unbind_keys();
    unbind_signals();

    fini_grab_interface();

    if (!is_shutting_down()) {
        // Destroy all workspaces, which will destroy all managed nodes and
        // detach custom data from the managed views.
        workspaces.workspaces.clear();
    }

    output->workspace->set_workspace_implementation(nullptr, true);
}

Swayfire::~Swayfire() = default;

DECLARE_WAYFIRE_PLUGIN(Swayfire)
