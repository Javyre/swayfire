#include "swayfire.hpp"
#include "grab.hpp"
#include <algorithm>
#include <functional>
#include <iterator>
#include <memory>
#include <wayfire/config/types.hpp>
#include <wayfire/core.hpp>
#include <wayfire/geometry.hpp>
#include <wayfire/util/log.hpp>

// nonwf

// TODO: use wf::get_view_main_workspace once we upgrade to wayfire > 0.7
wf::point_t nonwf::get_view_workspace(wayfire_view view, output_ref_t output) {
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
                                                 wf::point_t wsid,
                                                 output_ref_t output) {

    auto og = output->get_screen_size();
    auto curr = output->workspace->get_current_workspace();

    geo.x += og.width * (wsid.x - curr.x);
    geo.y += og.height * (wsid.y - curr.y);

    return geo;
}

wf::point_t nonwf::geometry_center(wf::geometry_t geo) {
    return {
        (int)std::floor((geo.x + geo.width) / 2.0f),
        (int)std::floor((geo.y + geo.height) / 2.0f),
    };
}

// node_parent_interface_t

split_node_ref_t node_parent_interface_t::as_split_node() {
    return dynamic_cast<split_node_t *>(this);
}

// node_interface_t

split_node_ref_t node_interface_t::as_split_node() {
    return dynamic_cast<split_node_t *>(this);
}

view_node_ref_t node_interface_t::as_view_node() {
    return dynamic_cast<view_node_t *>(this);
}

node_t node_interface_t::find_floating_parent() {
    if (get_floating())
        return this;

    if (auto p = parent->as_split_node())
        return p->find_floating_parent();

    return nullptr;
}

// view_node_t

void view_node_t::set_floating(bool fl) {
    if (!floating && fl)
        view->request_native_size();

    floating = fl;
    view->set_tiled(fl ? 0 : wf::TILED_EDGES_ALL);
}

void view_node_t::set_wsid(wf::point_t wsid) { this->wsid = wsid; }

wf::geometry_t view_node_t::get_geometry() {
    geometry = view->get_wm_geometry();
    return geometry;
}

void view_node_t::set_geometry(wf::geometry_t geo) {
    geometry = geo;
    view->set_geometry(nonwf::local_to_relative_geometry(geo, wsid, output));
}

split_node_ref_t view_node_t::replace_with_split() {
    if (prefered_split_type) {
        auto new_parent =
            std::make_unique<split_node_t>(get_geometry(), output);
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

node_parent_t view_node_t::get_active_parent_node() {
    if (auto split = replace_with_split())
        return split;
    else
        return parent;
}

// split_node_t

void split_node_t::insert_child_at(std::vector<owned_node_t>::iterator at,
                                   owned_node_t node) {
    node->parent = this;

    float shrink_ratio = (float)children.size() / (float)(children.size() + 1);
    float total_ratio = 0;

    for (auto i = children_ratios.begin(); i != children_ratios.end(); i++) {
        *i *= shrink_ratio;
        total_ratio += *i;
    }
    children_ratios.insert(children_ratios.begin() +
                               std::distance(children.begin(), at),
                           1.0f - total_ratio);

    children.insert(at, std::move(node));
    refresh_geometry();
}

void split_node_t::insert_child_front(owned_node_t node) {
    insert_child_at(children.begin(), std::move(node));
}

void split_node_t::insert_child_back(owned_node_t node) {
    insert_child_at(children.end(), std::move(node));
}

void split_node_t::insert_child_front_of(node_t of, owned_node_t node) {
    auto child = find_child(of);
    if (child == children.end())
        LOGE("Node ", of, " not found in split node: ", this);

    insert_child_at(child, std::move(node));
}

void split_node_t::insert_child_back_of(node_t of, owned_node_t node) {
    auto child = find_child(of);
    if (child == children.end())
        LOGE("Node ", of, " not found in split node: ", this);

    insert_child_at(child + 1, std::move(node));
}

std::vector<owned_node_t>::iterator split_node_t::find_child(node_t node) {
    return std::find_if(children.begin(), children.end(),
                        [&](auto &c) { return c.get() == node.get(); });
}

owned_node_t split_node_t::remove_child(node_t node) {
    auto child = find_child(node);
    if (child == children.end())
        LOGE("Node ", node, " not found in split node: ", this);

    children_ratios.erase(children_ratios.begin() +
                          std::distance(children.begin(), child));

    if (!children_ratios.empty()) {
        float grow_ratio = (float)(children_ratios.size() + 1) /
                           (float)(children_ratios.size());
        float total_ratio = 0;
        for (auto i = children_ratios.begin(); i != children_ratios.end() - 1;
             i++) {
            *i *= grow_ratio;
            total_ratio += *i;
        }
        children_ratios.back() = 1.0f - total_ratio;
    }

    auto owned_node = std::move(*child);
    children.erase(child);

    if (children.empty()) {
        active_child = 0;
    } else {
        active_child = std::clamp(active_child, (uint32_t)0,
                                  (uint32_t)(children.size() - 1));
    }

    refresh_geometry();

    owned_node->parent = nullptr;

    return owned_node;
}

void split_node_t::set_active_child(node_t node) {
    auto child = find_child(node);
    if (child == children.end()) {
        LOGE("Node ", node, " not found in split node: ", this);
        return;
    }

    active_child = std::distance(children.begin(), child);

    parent->set_active_child(this);
}

node_parent_t split_node_t::get_active_parent_node() { return this; }

owned_node_t split_node_t::swap_child(node_t node, owned_node_t other) {
    auto child = find_child(node);
    if (child == children.end())
        LOGE("Node ", node, " not found in split node: ", this);

    other->set_geometry((*child)->get_geometry());
    other->parent = this;

    (*child).swap(other);

    return other;
}

node_t split_node_t::get_last_active_node() {
    if (children.empty())
        return nullptr;

    auto &child = children.at(active_child);
    if (auto split = child->as_split_node()) {
        return split->get_last_active_node();
    } else {
        return child.get();
    }
}

node_t split_node_t::get_adjacent(node_t node, direction_t dir) {
    auto child = find_child(node);
    if (child == children.end())
        LOGE("Node ", node, " not found in split node: ", this);

    switch (split_type) {
    case split_type_t::VSPLIT:
    case split_type_t::TABBED: {
        switch (dir) {
#define PREV_NODE                                                              \
    child == children.begin() ? parent->get_adjacent(this, dir)                \
                              : (*(child - 1)).get()
#define NEXT_NODE                                                              \
    child == (children.end() - 1) ? parent->get_adjacent(this, dir)            \
                                  : (*(child + 1)).get()
        case direction_t::LEFT:
            return PREV_NODE;
        case direction_t::RIGHT:
            return NEXT_NODE;
        case direction_t::UP:
        case direction_t::DOWN:
            return parent->get_adjacent(this, dir);
        }
    }
    case split_type_t::HSPLIT:
    case split_type_t::STACKED: {
        switch (dir) {
        case direction_t::LEFT:
        case direction_t::RIGHT:
            return parent->get_adjacent(this, dir);
        case direction_t::UP:
            return PREV_NODE;
        case direction_t::DOWN:
            return NEXT_NODE;
#undef NEXT_NODE
#undef PREV_NODE
        }
        break;
    }
    }
}

split_node_ref_t split_node_t::find_parent_split(bool horiz) {
    auto p = parent->as_split_node();
    bool horizontal = false;
    bool vertical = false;
    while (p) {
        switch (p->split_type) {
        case split_type_t::VSPLIT:
        case split_type_t::TABBED:
            horizontal = true;
            break;
        case split_type_t::HSPLIT:
        case split_type_t::STACKED:
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

bool split_node_t::move_child_outside(node_iter_t child, direction_t dir) {
    if (auto adj = parent->get_adjacent(this, dir)) {
        if (auto adj_split = adj->parent->as_split_node()) {
            switch (dir) {
            case direction_t::LEFT:
            case direction_t::UP:
                adj_split->insert_child_back_of(adj, remove_child(*child));
                break;
            case direction_t::RIGHT:
            case direction_t::DOWN:
                adj_split->insert_child_front_of(adj, remove_child(*child));
                break;
            }
            return true;
        }
    } else {
        switch (dir) {
        case direction_t::LEFT:
            if (auto p = find_parent_split(true)) {
                p->insert_child_front(remove_child(*child));
                return true;
            }
            break;
        case direction_t::RIGHT:
            if (auto p = find_parent_split(true)) {
                p->insert_child_back(remove_child(*child));
                return true;
            }
            break;
        case direction_t::UP:
            if (auto p = find_parent_split(false)) {
                p->insert_child_front(remove_child(*child));
                return true;
            }
            break;
        case direction_t::DOWN:
            if (auto p = find_parent_split(false)) {
                p->insert_child_back(remove_child(*child));
                return true;
            }
            break;
        }
    }
    return false;
}

bool split_node_t::move_child(node_t node, direction_t dir) {
    auto child = find_child(node);
    if (child == children.end())
        LOGE("Node ", node, " not found in split node: ", this);

#define MOVE_BACK()                                                            \
    {                                                                          \
        if (child == children.begin()) {                                       \
            return move_child_outside(child, dir);                             \
        } else {                                                               \
            if (auto adj_split = (*(child - 1))->as_split_node()) {            \
                adj_split->insert_child_back(remove_child(*child));            \
                return true;                                                   \
            } else if (auto adj_view = (*(child - 1))->as_view_node()) {       \
                if (auto adj_split = adj_view->replace_with_split()) {         \
                    adj_split->insert_child_back(remove_child(*child));        \
                    return true;                                               \
                }                                                              \
            }                                                                  \
            insert_child_front_of((*(child - 1)).get(), remove_child(*child)); \
            return true;                                                       \
        }                                                                      \
    }

#define MOVE_FORWARD()                                                         \
    {                                                                          \
        if (child == children.end() - 1) {                                     \
            return move_child_outside(child, dir);                             \
        } else {                                                               \
            if (auto adj_split = (*(child + 1))->as_split_node()) {            \
                adj_split->insert_child_front(remove_child(*child));           \
                return true;                                                   \
            } else if (auto adj_view = (*(child + 1))->as_view_node()) {       \
                if (auto adj_split = adj_view->replace_with_split()) {         \
                    adj_split->insert_child_front(remove_child(*child));       \
                    return true;                                               \
                }                                                              \
            }                                                                  \
            insert_child_back_of((*(child + 1)).get(), remove_child(*child));  \
            return true;                                                       \
        }                                                                      \
    }

    switch (split_type) {
    case split_type_t::VSPLIT:
    case split_type_t::TABBED: {
        switch (dir) {
        case direction_t::LEFT:
            MOVE_BACK();
            break;
        case direction_t::RIGHT:
            MOVE_FORWARD();
            break;
        case direction_t::UP:
        case direction_t::DOWN:
            return move_child_outside(child, dir);
        }
    }
    case split_type_t::HSPLIT:
    case split_type_t::STACKED: {
        switch (dir) {
        case direction_t::UP:
            MOVE_BACK();
            break;
        case direction_t::DOWN:
            MOVE_FORWARD();
            break;
        case direction_t::LEFT:
        case direction_t::RIGHT:
            return move_child_outside(child, dir);
        }
    }
    }
#undef MOVE_FORWARD
#undef MOVE_BACK
}

void split_node_t::set_floating(bool fl) { floating = fl; }

void split_node_t::set_wsid(wf::point_t wsid) {
    this->wsid = wsid;
    for (auto &child : children)
        child->set_wsid(wsid);
}

void split_node_t::set_geometry(wf::geometry_t geo) {
    switch (split_type) {
// distribute over dim1 and pos1
#define DISTRIBUTE(dim1, dim2, pos1, pos2)                                     \
    {                                                                          \
        auto total = 0;                                                        \
        unsigned int i = 0;                                                    \
        for (auto &child : children) {                                         \
            auto subgeo = child->get_geometry();                               \
                                                                               \
            subgeo.pos1 = geo.pos1 + total;                                    \
            subgeo.pos2 = geo.pos2;                                            \
                                                                               \
            if (i == children.size() - 1) {                                    \
                subgeo.dim1 = geo.dim1 - total;                                \
            } else {                                                           \
                subgeo.dim1 = geo.dim1 * children_ratios[i];                   \
                total += subgeo.dim1;                                          \
            }                                                                  \
            subgeo.dim2 = geo.dim2;                                            \
                                                                               \
            child->set_geometry(subgeo);                                       \
            i++;                                                               \
        }                                                                      \
    }
    case split_type_t::VSPLIT:
        DISTRIBUTE(width, height, x, y);
        break;
    case split_type_t::HSPLIT:
        DISTRIBUTE(height, width, y, x);
        break;
#undef DISTRIBUTE
    case split_type_t::TABBED: {
        for (auto &child : children)
            child->set_geometry(geo);
        break;
    }
    case split_type_t::STACKED: {
        for (auto &child : children)
            child->set_geometry(geo);
        break;
    }
    }
    geometry = geo;
}

// workspace_t

void workspace_t::set_active_node(node_t node) {
    if (auto vnode = node->as_view_node())
        if (!vnode->view->activated)
            vnode->view->focus_request();

    node->parent->set_active_child(node);

    if (!node->get_floating())
        active_tiled_node = node;

    active_node = node;
}

node_t workspace_t::get_active_node() { return active_node; }

node_parent_t workspace_t::get_active_parent_node() {
    return get_active_node()->get_active_parent_node();
}

void workspace_t::insert_floating_node(owned_node_t node) {
    node->set_floating(true);
    node->set_wsid(wsid);
    node->parent = this;
    floating_nodes.push_back(std::move(node));
}

std::vector<owned_node_t>::iterator workspace_t::find_floating(node_t node) {
    return std::find_if(floating_nodes.begin(), floating_nodes.end(),
                        [&](auto &c) { return c.get() == node.get(); });
}

owned_node_t workspace_t::remove_floating_node(node_t node) {
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

    owned_node->set_floating(false);

    return owned_node;
}

owned_node_t workspace_t::swap_floating_node(node_t node, owned_node_t other) {
    auto &fl = floating_nodes;
    auto child = find_floating(node);
    if (child == fl.end()) {
        LOGE("Node not floating in ", this, ": ", node);
        return nullptr;
    }

    other->set_floating(true);
    other->set_wsid(wsid);
    other->parent = this;
    other->set_geometry((*child)->get_geometry());

    (*child).swap(other);

    return other;
}

node_t workspace_t::get_active_floating_node() {
    if (floating_nodes.empty())
        return nullptr;

    return floating_nodes.at(active_floating).get();
}

node_t workspace_t::get_active_tiled_node() { return active_tiled_node; }

owned_node_t workspace_t::swap_tiled_root(std::unique_ptr<split_node_t> other) {
    auto ret = std::move(tiled_root);

    tiled_root = std::move(other);
    tiled_root->set_floating(false);
    tiled_root->set_wsid(wsid);
    tiled_root->parent = this;

    return ret;
}

void workspace_t::insert_tiled_node(owned_node_t node) {
    node->set_floating(false);
    node->set_wsid(wsid);
    auto parent = get_active_parent_node();

    parent->insert_child(std::move(node));
}

owned_node_t workspace_t::remove_tiled_node(node_t node) {
    if (node->get_floating() || node->get_wsid() != wsid) {
        LOGE("Node not tiled in ", this, ": ", node);
        return nullptr;
    }

    auto parent = node->parent;
    owned_node_t ret;
    if (auto parent = node->parent) {
        ret = parent->remove_child(node);
    } else if (node.get() == tiled_root.get()) {
        ret = swap_tiled_root(std::make_unique<split_node_t>(geometry, output));
    } else {
        LOGE("Node not tiled in ", this, ": ", node);
        return nullptr;
    }

    if (ret.get() == get_active_tiled_node().get()) {
        active_tiled_node = parent->get_last_active_node();
    }

    ret->set_floating(false);
    return ret;
}

owned_node_t workspace_t::remove_node(node_t node) {
    return node->get_floating() ? remove_floating_node(node)
                                : remove_tiled_node(node);
}

void workspace_t::insert_child(owned_node_t node) {
    node->set_floating(false);
    node->set_wsid(wsid);

    tiled_root->insert_child(std::move(node));
}

owned_node_t workspace_t::remove_child(node_t node) {
    owned_node_t ret;

    if (node->get_floating()) {
        // floating nodes are always direct children of the workspace
        ret = remove_floating_node(node);
    } else if (node.get() == tiled_root.get()) {
        ret = swap_tiled_root(std::make_unique<split_node_t>(geometry, output));
    } else {
        LOGE("Node is not a direct child of ", this, ": ", node);
        return nullptr;
    }

    if (node.get() == get_active_node().get())
        set_active_node(tiled_root);

    return ret;
}

owned_node_t workspace_t::swap_child(node_t node, owned_node_t other) {
    if (node->get_floating()) {
        // floating nodes are always direct children of the workspace
        return swap_floating_node(node, std::move(other));
    } else if (node.get() == tiled_root.get()) {
        LOGE("Cannot swap node with tiled-root node of ", this);
        return nullptr;
    } else {
        LOGE("Node is not a direct child of ", this, ": ", node);
        return nullptr;
    }
}

void workspace_t::set_active_child(node_t node) {
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

void workspace_t::set_workarea(wf::geometry_t geo) {
    geometry = geo;
    tiled_root->set_geometry(geo);
}

void workspace_t::toggle_tile_node(node_t node) {
    LOGD("toggling tiling for ", node);

    if (node->get_floating()) {
        insert_tiled_node(remove_floating_node(node));
    } else {
        insert_floating_node(remove_tiled_node(node));
    }
}

node_t workspace_t::get_last_active_node() { return active_node; }

node_t workspace_t::get_adjacent(node_t node, direction_t dir) {
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
        case direction_t::LEFT:
            CLOSEST(x, <);
        case direction_t::RIGHT:
            CLOSEST(x, >);
        case direction_t::UP:
            CLOSEST(y, <);
        case direction_t::DOWN:
            CLOSEST(y, >);
#undef CLOSEST
        }
    }
}

bool workspace_t::move_child(node_t node, direction_t dir) {
    if (node->get_floating()) {
        auto geo = node->get_geometry();

        switch (dir) {
        case direction_t::LEFT:
            geo.x -= FLOATING_MOVE_STEP;
            break;
        case direction_t::RIGHT:
            geo.x += FLOATING_MOVE_STEP;
            break;
        case direction_t::UP:
            geo.y -= FLOATING_MOVE_STEP;
            break;
        case direction_t::DOWN:
            geo.y += FLOATING_MOVE_STEP;
            break;
        }

        node->set_geometry(geo);
        return true;
    }
    return false;
}

// workspaces_t

void workspaces_t::update_dims(wf::dimensions_t ndims, wf::geometry_t geo,
                               output_ref_t output) {

    workspaces.resize(ndims.width);

    auto x = 0;
    for (auto &col : workspaces) {
        if (col.size() == (uint32_t)ndims.height) {
            // noop
        } else if (col.size() < (uint32_t)ndims.height) {
            col.reserve(ndims.height);
            for (int32_t y = col.size(); y < ndims.height; y++) {
                col.push_back(std::unique_ptr<workspace_t>(
                    new workspace_t({x, y}, geo, output)));
            }
        } else {
            col.erase(col.begin() + ndims.height, col.end());
        }
        x++;
    }
}

workspace_ref_t workspaces_t::get(wf::point_t ws) {
    return workspaces.at(ws.x).at(ws.y).get();
}

void workspaces_t::for_each(std::function<void(workspace_ref_t)> fun) {
    for (auto &col : workspaces)
        for (auto &ws : col)
            fun(ws.get());
}

// swayfire_t

std::unique_ptr<view_node_t> swayfire_t::init_view_node(wayfire_view view) {
    auto node = std::make_unique<view_node_t>(view, output);
    view->store_data<view_data_t>(std::make_unique<view_data_t>(node));

    LOGD("New view-node for ", view->to_string(), ": ", node->to_string());
    return node;
}

void swayfire_t::fini_view(wayfire_view view) {
    if (view->has_data<view_data_t>()) {
        auto vdata = view->get_data<view_data_t>();
        auto node = vdata->node;

        LOGD("Fini for: ", node);

        auto owned_node = node->parent->remove_child(node);
        auto vnode = static_cast<view_node_t *>(owned_node.release());
        vnode->view->erase_data<view_data_t>();

        delete vnode;
    }
}

void swayfire_t::bind_signals() {
    wf::get_core().connect_signal("shutdown", &on_shutdown);
    output->connect_signal("view-layer-attached", &on_view_attached);
    output->connect_signal("view-unmapped", &on_view_unmapped);
    output->connect_signal("view-focused", &on_view_focused);
    output->connect_signal("workarea-changed", &on_workarea_changed);
}

void swayfire_t::unbind_signals() {
    output->disconnect_signal(&on_workarea_changed);
    output->disconnect_signal(&on_view_focused);
    output->disconnect_signal(&on_view_unmapped);
    output->disconnect_signal(&on_view_attached);
    wf::get_core().disconnect_signal(&on_shutdown);
}

void swayfire_t::init() {
    output->workspace->set_workspace_implementation(
        std::make_unique<swayfire_workspace_implementation_t>(), true);

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
        if (auto vdata = active_view->get_data<view_data_t>()) {
            auto node = vdata->node;

            workspaces.get(node->get_wsid())->set_active_node(node);
        }
    }

    init_grab_interface();

    bind_signals();
    bind_keys();
}

void swayfire_t::fini() {
    LOGD("==== fini ====");

    unbind_keys();
    unbind_signals();

    fini_grab_interface();

    if (!is_shutting_down()) {
        auto views = output->workspace->get_views_in_layer(wf::ALL_LAYERS);

        for (auto view : views)
            fini_view(view);
    }

    output->workspace->set_workspace_implementation(nullptr, true);
}

swayfire_t::~swayfire_t() = default;

DECLARE_WAYFIRE_PLUGIN(swayfire_t)
