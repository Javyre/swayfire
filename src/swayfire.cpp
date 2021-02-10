#include "swayfire.hpp"
#include <functional>
#include <memory>
#include <wayfire/config/types.hpp>
#include <wayfire/core.hpp>
#include <wayfire/geometry.hpp>
#include <wayfire/util/log.hpp>

// nonwf

// TODO: use wf::get_view_main_workspace once we upgrade to wayfire > 0.7
wf::point_t nonwf::get_view_workspace(wayfire_view view, output_ref_t output)
{
    auto og = output->get_screen_size();

    auto wm = view->transform_region(view->get_wm_geometry());
    auto x = (int)std::floor((wm.x + wm.width / 2.0) / og.width);
    auto y = (int)std::floor((wm.y + wm.height / 2.0) / og.height);

    auto dims = output->workspace->get_workspace_grid_size();
    auto curr = output->workspace->get_current_workspace();

    // convert relative coords to nearest absolute wsid coords
    x = std::clamp(x + curr.x, 0, dims.width-1);
    y = std::clamp(y + curr.y, 0, dims.height-1);

    return {x, y};
}

wf::geometry_t nonwf::local_to_relative_geometry(
        wf::geometry_t geo,
        wf::point_t wsid,
        output_ref_t output) {

    auto og = output->get_screen_size();
    auto curr = output->workspace->get_current_workspace();

    geo.x += og.width * (wsid.x - curr.x);
    geo.y += og.height * (wsid.y - curr.y);

    return geo;
}

// node_parent_interface_t

split_node_ref_t node_parent_interface_t::as_split_node() {
    return dynamic_cast<split_node_t *>(this);
}

// view_node_t

void view_node_t::set_floating(bool fl) {
    if (!floating && fl)
        view->request_native_size();

    floating = fl;
    view->set_tiled(fl ? 0 : wf::TILED_EDGES_ALL);
}

void view_node_t::set_wsid(wf::point_t wsid) {
    this->wsid = wsid;
}

void view_node_t::set_geometry(wf::geometry_t geo) {
    geometry = geo;
    view->set_geometry(nonwf::local_to_relative_geometry(geo, wsid, output));
}

/* node_t view_node_t::deepest_active_node() { */
/*     return this; */
/* } */

node_parent_t view_node_t::get_active_parent_node() {
    if (prefered_split_type) {
        auto new_parent = std::make_unique<split_node_t>(get_geometry(), output);
        auto new_parent_ref = new_parent.get();
        new_parent->split_type = *prefered_split_type;
        auto owned_self = parent->swap_child(this, std::move(new_parent));
        new_parent_ref->insert_child_back(std::move(owned_self));

        prefered_split_type = {};
        return new_parent_ref;
    } else {
        return parent;
    }
}

// split_node_t

void split_node_t::insert_child_front(owned_node_t node) {
    node->parent = this;

    float shrink_ratio = (float)children.size() / (float)(children.size()+1);
    float total_ratio = 0;

    for (auto i = children_ratios.begin(); i != children_ratios.end(); i++) {
        *i *= shrink_ratio;
        total_ratio += *i;
    }
    children_ratios.insert(children_ratios.begin(), 1.0f-total_ratio);

    children.insert(children.begin(), std::move(node));
    refresh_geometry();
}

void split_node_t::insert_child_back(owned_node_t node) {
    node->parent = this;

    float shrink_ratio = (float)children.size() / (float)(children.size()+1);
    float total_ratio = 0;

    for (auto i = children_ratios.begin(); i != children_ratios.end(); i++) {
        *i *= shrink_ratio;
        total_ratio += *i;
    }
    children_ratios.push_back(1.0f-total_ratio);

    children.push_back(std::move(node));
    LOGD("children: ", children.size(), ",", children_ratios.size());
    refresh_geometry();
}

owned_node_t split_node_t::remove_child(node_t node) {
    auto child = std::find_if(children.begin(), children.end(), [&](auto &c){
        return c.get() == node.get();
    });

    if (child == children.end())
        LOGE("Node ", node, " not found in split node: ", this);

    children_ratios.erase(children_ratios.begin() + std::distance(children.begin(), child));

    if (!children_ratios.empty()) {
        float grow_ratio = (float)(children_ratios.size()+1) / (float)(children_ratios.size());
        float total_ratio = 0;
        for (auto i = children_ratios.begin(); i != children_ratios.end()-1; i++) {
            *i *= grow_ratio;
            total_ratio += *i;
        }
        children_ratios.back() = 1.0f-total_ratio;
    }

    auto owned_node = std::move(*child);
    children.erase(child);

    if (children.empty()) {
        active_child = 0;
    } else {
        active_child = 
            std::clamp(active_child, (uint32_t)0, (uint32_t)(children.size()-1));
    }

    refresh_geometry();

    owned_node->parent = nullptr;

    return owned_node;
}

node_parent_t split_node_t::get_active_parent_node() {
    return this;
}

owned_node_t split_node_t::swap_child(node_t node, owned_node_t other) {
    auto child = std::find_if(children.begin(), children.end(), [&](auto &c){
        return c.get() == node.get();
    });

    if (child == children.end())
        LOGE("Node ", node, " not found in split node: ", this);

    other->set_geometry((*child)->get_geometry());

    (*child).swap(other);

    return other;
}

void split_node_t::set_floating(bool fl) {
    floating = fl;
    /* for (auto &child : children) */
    /*     child->set_floating(fl); */
}

void split_node_t::set_wsid(wf::point_t wsid) {
    this->wsid = wsid;
    for (auto &child : children)
        child->set_wsid(wsid);
}

void split_node_t::set_geometry(wf::geometry_t geo) {
    switch (split_type) {
// distribute over dim1 and pos1
#define DISTRIBUTE(dim1, dim2, pos1, pos2)                \
{                                                         \
    auto total = 0;                                       \
    unsigned int i = 0;                                   \
    for (auto &child : children) {                        \
        auto subgeo = child->get_geometry();              \
                                                          \
        subgeo.pos1 = geo.pos1 + total;                   \
        subgeo.pos2 = geo.pos2;                           \
                                                          \
        if (i == children.size()-1) {                     \
            subgeo.dim1 = geo.dim1 - total;               \
        } else {                                          \
            subgeo.dim1 = geo.dim1 * children_ratios[i];  \
            total += subgeo.dim1;                         \
        }                                                 \
        subgeo.dim2 = geo.dim2;                           \
                                                          \
        child->set_geometry(subgeo);                      \
        i++;                                              \
    }                                                     \
}
        case split_type_t::VSPLIT: 
            DISTRIBUTE(width, height, x, y);
            break;
        case split_type_t::HSPLIT: 
            DISTRIBUTE(height, width, y, x);
            break;
#undef DISTRIBUTE
        case split_type_t::TABBED:
            {
                for (auto &child : children)
                    child->set_geometry(geo);
                break;
            }
        case split_type_t::STACKED: 
            {
                for (auto &child : children)
                    child->set_geometry(geo);
                break;
            }
    }
    geometry = geo;
}

/* node_t split_node_t::deepest_active_node() { */
/*     return children.empty() ? this : children.at(active_child).get(); */
/* } */

// workspace_t

node_parent_t workspace_t::get_active_parent_node() {
    return active_node->get_active_parent_node();
    /* if (auto split_node = dynamic_cast<split_node_t *>(active_node.get())) { */
    /*     return split_node; */
    /* } else if (auto spli) { */
    /*     if (active_node->prefered_split_type) */
    /* } */

    /* if (auto root = dynamic_cast<split_node_t *>(tiled_root.get())) { */
    /*     auto deepest = root->deepest_active_node(); */
    /*     if (auto deepest_split = dynamic_cast<split_node_t *>(deepest.get())) { */
    /*         return deepest_split; */
    /*     } else { */
    /*         return deepest->parent; */
    /*     } */
    /* } else { */
    /*     if (tiled_root) { */
    /*         auto new_root = std::make_unique<split_node_t>(geometry); */
    /*         new_root->insert_child_back(std::move(tiled_root)); */

    /*         tiled_root = std::move(new_root); */
    /*     } else { */
    /*         tiled_root = std::make_unique<split_node_t>(geometry); */
    /*     } */

    /*     return nonstd::make_observer<split_node_t>( */
    /*             static_cast<split_node_t *>(tiled_root.get())); */
    /* } */
}

void workspace_t::insert_floating_node(owned_node_t node) {
    node->set_floating(true);
    node->set_wsid(wsid);
    node->parent = this;
    floating_nodes.push_back(std::move(node));
}

owned_node_t workspace_t::remove_floating_node(node_t node) {
    auto &fl = floating_nodes;
    auto child = std::find_if(fl.begin(), fl.end(), [&](auto &c){
        return c.get() == node.get();
    });

    if (child == fl.end()) {
        LOGE("Node not floating in ", this, ": ", node);
        return nullptr;
    }

    auto owned_node = std::move(*child);

    fl.erase(child);

    owned_node->set_floating(false);

    return owned_node;
}

owned_node_t workspace_t::swap_floating_node(node_t node, owned_node_t other) {
    auto &fl = floating_nodes;
    auto child = std::find_if(fl.begin(), fl.end(), [&](auto &c){
        return c.get() == node.get();
    });

    if (child == fl.end()) {
        LOGE("Node not floating in ", this, ": ", node);
        return nullptr;
    }

    other->set_floating(true);
    other->set_wsid(wsid);
    other->set_geometry((*child)->get_geometry());

    (*child).swap(other);

    return other;
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

    owned_node_t owned_node;
    if (auto parent = node->parent) {
        owned_node = parent->remove_child(node);
    } else if (node.get() == tiled_root.get()) {
        owned_node = std::move(tiled_root);
        tiled_root = std::make_unique<split_node_t>(geometry, output);
    } else {
        LOGE("Node not tiled in ", this, ": ", node);
        return nullptr;
    }

    owned_node->set_floating(false);
    return owned_node;
}

owned_node_t workspace_t::remove_node(node_t node) {
    return node->get_floating() ? 
        remove_floating_node(node) : remove_tiled_node(node);
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
        ret = std::move(tiled_root);
        tiled_root = std::make_unique<split_node_t>(geometry, output);
    } else {
        LOGE("Node is not a direct child of ", this, ": ", node);
        return nullptr;
    }

    if (node.get() == active_node.get())
        active_node = tiled_root;

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

void workspace_t::toggle_split_direction_node(node_t node) {
    if (auto parent = node->parent->as_split_node()) {
        LOGD("Toggling split dir: ", parent);

        parent->split_type = (parent->split_type == split_type_t::HSPLIT)
            ? split_type_t::VSPLIT
            : split_type_t::HSPLIT;

        parent->refresh_geometry();
    }
}

// workspaces_t

void workspaces_t::update_dims(
        wf::dimensions_t ndims,
        wf::geometry_t geo,
        output_ref_t output
        ) {

    workspaces.resize(ndims.width);

    auto x = 0;
    for (auto &col : workspaces) {
        if (col.size() == (uint32_t)ndims.height) {
            // noop
        } else if (col.size() < (uint32_t)ndims.height) {
            col.reserve(ndims.height);
            for (int32_t y = col.size(); y < ndims.height; y++) {
                col.push_back(workspace_t({x, y}, geo, output));
            }
        } else {
            col.erase(col.begin()+ndims.height, col.end());
        }
        x++;
    }
}

workspace_t &workspaces_t::get(wf::point_t ws) {
    return workspaces.at(ws.x).at(ws.y);
}

void workspaces_t::for_each(std::function<void(workspace_t &)> fun) {
    for (auto &col : workspaces)
        for (auto &ws : col)
            fun(ws);
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

void swayfire_t::bind_keys() {
    output->add_key(key_toggle_tile, &on_toggle_tile);
    output->add_key(key_toggle_split_direction, &on_toggle_split_direction);
    output->add_key(key_set_want_vsplit, &on_set_want_vsplit);
    output->add_key(key_set_want_hsplit, &on_set_want_hsplit);
}

void swayfire_t::unbind_keys() {
    output->rem_binding(&on_set_want_hsplit);
    output->rem_binding(&on_set_want_vsplit);
    output->rem_binding(&on_toggle_split_direction);
    output->rem_binding(&on_toggle_tile);
}

void swayfire_t::init() {
    output->workspace->set_workspace_implementation(
            std::make_unique<swayfire_workspace_implementation_t>(), true);

    auto grid_dims = output->workspace->get_workspace_grid_size();

    workspaces.update_dims(grid_dims, output->workspace->get_workarea(), output);

    auto views = output->workspace->get_views_in_layer(wf::ALL_LAYERS);

    for (auto view : views) {
        if (view->role == wf::VIEW_ROLE_TOPLEVEL) {
            auto &ws = workspaces.get(nonwf::get_view_workspace(view, output));
            ws.insert_tiled_node(init_view_node(view));
        }
    }

    if (auto active_view = output->get_active_view()) {
        if (auto vdata = active_view->get_data<view_data_t>()) {
            auto node = vdata->node;

            workspaces.get(node->get_wsid()).active_node = node;
        }
    }

    bind_signals();
    bind_keys();
}

void swayfire_t::fini() {
    LOGD("==== fini ====");

    unbind_keys();
    unbind_signals();

    if (!is_shutting_down()) {
        auto views = output->workspace->get_views_in_layer(wf::ALL_LAYERS);

        for (auto view : views)
            fini_view(view);
    }

    output->workspace->set_workspace_implementation(nullptr, true);
}

DECLARE_WAYFIRE_PLUGIN(swayfire_t)
