#include "core.hpp"
#include "../nonstd.hpp"
#include "grab.hpp"
#include "plugin.hpp"

// nonwf

// TODO: use wf::get_view_main_workspace once we upgrade to wayfire > 0.7
wf::point_t nonwf::get_view_workspace(wayfire_view view, bool with_transform) {
    const auto output = view->get_output();
    const auto og = output->get_screen_size();

    const auto wm = with_transform
                        ? view->transform_region(view->get_wm_geometry())
                        : view->get_wm_geometry();
    auto x = (int)std::floor((wm.x + wm.width / 2.0) / og.width);
    auto y = (int)std::floor((wm.y + wm.height / 2.0) / og.height);

    const auto dims = output->workspace->get_workspace_grid_size();
    const auto curr = output->workspace->get_current_workspace();

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
        (int)std::floor((double)(geo.x + geo.width) / 2.0),
        (int)std::floor((double)(geo.y + geo.height) / 2.0),
    };
}

wf::dimensions_t nonwf::min(const wf::dimensions_t &a,
                            const wf::dimensions_t &b) {
    return {
        std::min(a.width, b.width),
        std::min(a.height, b.height),
    };
}

wf::dimensions_t nonwf::max(const wf::dimensions_t &a,
                            const wf::dimensions_t &b) {
    return {
        std::max(a.width, b.width),
        std::max(a.height, b.height),
    };
}

// INodeParent

SplitNodeRef INodeParent::as_split_node() {
    return dynamic_cast<SplitNode *>(this);
}

// INode

SplitNodeRef INode::as_split_node() { return dynamic_cast<SplitNode *>(this); }

ViewNodeRef INode::as_view_node() { return dynamic_cast<ViewNode *>(this); }

void INode::notify_initialized() {
    if (initialized)
        return;

    initialized = true;
    LOGD(this, " initialized");
    on_initialized();
}

void INode::add_padding(Padding padding) { this->padding += padding; }

void INode::set_floating(bool fl) {
    if (!floating && fl)
        set_geometry(floating_geometry);

    if (floating && !fl)
        floating_geometry = get_geometry();

    floating = fl;
};

void INode::set_active() { get_ws()->set_active_node(this); }

void INode::tile_request(const bool tile) {
    get_ws()->tile_request(this, tile);
}

bool INode::move(Direction dir) {
    auto old_parent = parent;
    if (!parent->move_child(this, dir))
        return false;

    if (old_parent.get() != get_ws()->tiled_root.node.get()) {
        if (auto old_parent_split = old_parent->as_split_node()) {
            if (old_parent_split->empty()) {
                // reset_active = true, since we're possibly destroying the
                // active node here.
                (void)get_ws()->remove_node(old_parent_split, true);
            } else {
                old_parent_split->try_downgrade();
            }
        }
    }

    // Refresh the active node to trigger any node activation tree updates.
    // (e.g. bring_to_front() on the new root parent and set_active_child() on
    // the new parents).
    if (this == get_ws()->get_active_node().get())
        get_ws()->set_active_node(this);

    return true;
}

Node INode::find_root_parent() {
    if (auto psplit = parent->as_split_node())
        return psplit->find_root_parent();

    assert(parent.get() == get_ws().get() &&
           "non-split node parent must be the node's workspace.");
    return this;
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

void ViewGeoEnforcer::ref_disable() { disabled++; }

void ViewGeoEnforcer::unref_disable() {
    assert(disabled);
    disabled--;
}

void ViewGeoEnforcer::update_transformer() {
    auto curr = view->get_wm_geometry();

    if (curr.width <= 0 && curr.height <= 0)
        return;

    auto geo = view_node->get_inner_geometry();

    auto output = view_node->ws->output;
    auto wsid = view_node->ws->wsid;
    auto curr_wsid = output->workspace->get_current_workspace();
    if (wsid != curr_wsid)
        geo = nonwf::local_to_relative_geometry(geo, wsid, curr_wsid, output);

    if (disabled || curr == geo) {
        scale_x = 1;
        scale_y = 1;
        translation_x = 0;
        translation_y = 0;

        view_node->push_disable_on_geometry_changed();
        view->damage();
        view_node->pop_disable_on_geometry_changed();
        return;
    }

    scale_x = (float)geo.width / (float)curr.width;
    scale_y = (float)geo.height / (float)curr.height;

    translation_x = (float)geo.x - (float)curr.x +
                    ((float)geo.width - (float)curr.width) / 2.0f;
    translation_y = (float)geo.y - (float)curr.y +
                    ((float)geo.height - (float)curr.height) / 2.0f;

    view_node->push_disable_on_geometry_changed();
    view->damage();
    view_node->pop_disable_on_geometry_changed();
}

// ViewNode

ViewNode::ViewNode(wayfire_view view) : view(view) {
    auto ge = std::make_unique<ViewGeoEnforcer>(this);
    geo_enforcer = ge.get();
    view->add_transformer(std::move(ge));

    geometry = view->get_wm_geometry() + padding;
    floating_geometry = geometry;

    view->connect_signal("mapped", &on_mapped);
    view->connect_signal("unmapped", &on_unmapped);
    view->connect_signal("geometry-changed", &on_geometry_changed);
}

ViewNode::~ViewNode() {
    LOGD("Destroying ", this);
    ViewNodeSignalData data = {};
    data.node = this;
    emit_signal("detached", &data);
    get_ws()->output->emit_signal("swf-view-node-detached", &data);

    view->disconnect_signal(&on_geometry_changed);
    view->disconnect_signal(&on_unmapped);
    view->disconnect_signal(&on_mapped);

    view->pop_transformer(geo_enforcer);

    view->erase_data<ViewData>();
}

void ViewNode::on_unmapped_impl() {
    // ws might get unset on remove_child so we must save it.
    auto ws = this->ws;

    (void)ws->remove_node(this);
    // view node dies here.
}

void ViewNode::on_geometry_changed_impl() {
    if (disable_on_geometry_changed == 0) {
        if (get_floating()) {
            const auto curr_wsid =
                get_ws()->output->workspace->get_current_workspace();
            auto ngeo = nonwf::local_to_relative_geometry(
                view->get_wm_geometry(), curr_wsid, get_ws()->wsid,
                get_ws()->output);
            set_geometry(ngeo + padding);
        } else {
            const auto new_ws =
                get_ws()->plugin->get_view_workspace(view, false);

            if (new_ws->wsid == get_ws()->wsid)
                return;

            LOGD(this, ": moving from ", get_ws(), " to ", new_ws);

            auto const old_ws = get_ws();
            auto owned_self = old_ws->remove_tiled_node(this);
            new_ws->insert_tiled_node(std::move(owned_self));
        }
    }
}

void ViewNode::set_floating(bool fl) {
    INode::set_floating(fl);
    view->set_tiled(fl ? 0 : wf::TILED_EDGES_ALL);
}

void ViewNode::on_initialized() {
    // It's probably dangerous to send out this signal and start changing
    // unmapped views' geomtries.
    assert(view->is_mapped());

    ViewNodeSignalData data = {};
    data.node = this;
    ws->output->emit_signal("swf-view-node-attached", &data);
}

void ViewNode::set_geometry(const wf::geometry_t geo) {
    geometry = geo;
    if (pure_set_geo)
        return;

    auto inner = get_inner_geometry();

    auto curr_wsid = ws->output->workspace->get_current_workspace();
    if (ws->wsid != curr_wsid)
        inner = nonwf::local_to_relative_geometry(inner, ws->wsid, curr_wsid,
                                                  ws->output);

    push_disable_on_geometry_changed();
    view->set_geometry(inner);
    pop_disable_on_geometry_changed();

    geo_enforcer->update_transformer();
}

SplitNodeRef ViewNode::try_upgrade() {
    if (auto split_type = get_prefered_split_type()) {
        auto new_parent =
            std::make_unique<SplitNode>(get_geometry(), *split_type);
        auto new_parent_ref = new_parent.get();
        auto owned_self = parent->swap_child(this, std::move(new_parent));
        owned_self->set_floating(false);
        new_parent_ref->insert_child_back(std::move(owned_self));

        set_prefered_split_type(std::nullopt);
        return new_parent_ref;
    }
    return nullptr;
}

std::optional<SplitType> ViewNode::get_prefered_split_type() {
    return prefered_split_type;
}

void ViewNode::set_prefered_split_type(std::optional<SplitType> split_type) {
    if (split_type != prefered_split_type) {
        emit_signal("prefered-split-type-changed", nullptr);
        prefered_split_type = split_type;
    }
}

void ViewNode::set_sublayer(nonstd::observer_ptr<wf::sublayer_t> sublayer) {
    assert(sublayer.get() != nullptr);
    get_ws()->output->workspace->add_view_to_sublayer(view, sublayer);
}

void ViewNode::bring_to_front() {
    auto &wfws = get_ws()->output->workspace;
    wfws->bring_to_front(view);

    // TODO: remove this code when
    // https://github.com/WayfireWM/wayfire/pull/1173 gets released.
    // This damaging should be done by wayfire and not swayfire:
    auto views = wfws->get_views_on_workspace_sublayer(
        get_ws()->wsid, get_ws()->get_child_sublayer(find_root_parent()));

    for (auto v : views)
        v->damage();
}

void ViewNode::on_set_active() {
    for (auto view : view->enumerate_views())
        if (view->activated)
            return;

    get_ws()->output->focus_view(view, true);
}

NodeParent ViewNode::get_or_upgrade_to_parent_node() {
    if (auto split = try_upgrade())
        return split;
    else
        return parent;
}

void ViewNode::for_each_node(const std::function<void(Node)> &f) { f(this); }

// SplitNode

void SplitNode::sync_ratios_to_sizes() {
    assert("Cannot sync ratios to sizes when children are stacked." &&
           is_split());

    uint32_t total_size = 0;
    for (const auto &c : children)
        total_size += c.size;

    assert(total_size != 0);

    double total_ratio = 0;
    for (auto &c : children | nonstd::skip_last) {
        c.ratio = (double)c.size / (double)total_size;
        total_ratio += c.ratio;
    }

    assert(total_ratio < 1.0);
    assert(total_ratio >= 0.0);

    children.back().ratio = 1.0 - total_ratio;
}

void SplitNode::sync_sizes_to_ratios() {
    assert("Cannot sync sizes to ratios when children are stacked." &&
           is_split());

    const auto total_size = split_type == SplitType::VSPLIT
                                ? get_inner_geometry().width
                                : get_inner_geometry().height;

    auto size_left = total_size;
    for (auto &c : children | nonstd::skip_last) {
        c.size = (uint32_t)(c.ratio * (double)total_size);
        size_left -= c.size;
    }

    children.back().size = size_left;
}

void SplitNode::insert_child_at(SplitChildIter at, OwnedNode node) {
    node->parent = this;
    node->set_floating(false);
    node->set_ws(get_ws());
    node->set_sublayer(get_ws()->get_child_sublayer(find_root_parent()));
    node->notify_initialized();

    double total_ratio = 0;
    if (!children.empty()) {
        if (is_split())
            sync_ratios_to_sizes();

        double shrink_ratio =
            (double)children.size() / (double)(children.size() + 1);

        for (auto &c : children) {
            c.ratio *= shrink_ratio;
            total_ratio += c.ratio;
        }
    }

    SplitChild nchild;
    nchild.node = std::move(node);
    nchild.ratio = 1.0 - total_ratio;

    children.insert(at, std::move(nchild));

    if (is_split())
        sync_sizes_to_ratios();

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
    if (is_split())
        sync_ratios_to_sizes();

    auto owned_node = std::move(child->node);
    children.erase(child);

    if (children.empty()) {
        active_child = 0;
    } else {
        active_child = std::clamp(active_child, (uint32_t)0,
                                  (uint32_t)(children.size() - 1));
    }

    if (!children.empty()) {
        double grow_ratio =
            (double)(children.size() + 1) / (double)(children.size());

        double total_ratio = 0;
        for (auto &c : children | nonstd::skip_last) {
            c.ratio *= grow_ratio;
            total_ratio += c.ratio;
        }
        children.back().ratio = 1.0 - total_ratio;

        if (is_split())
            sync_sizes_to_ratios();
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

Node SplitNode::get_active_child() const { return child_at(active_child); }

void SplitNode::set_split_type(SplitType st) {
    if (is_split())
        was_vsplit = split_type == SplitType::VSPLIT;
    split_type = st;
    refresh_geometry();
}

Node SplitNode::try_downgrade() {
    if (children.size() == 1) {
        // Can only swap tiled_root of workspace with a split node.
        if (get_ws()->tiled_root.node.get() == this &&
            !children.front().node->as_split_node())
            return nullptr;

        auto only_child = remove_child_at(children.begin() + active_child);
        auto only_child_ref = only_child.get();

        if (auto vnode = only_child->as_view_node())
            vnode->set_prefered_split_type(split_type);

        bool was_active = get_ws()->get_active_node().get() == this;

        parent->swap_child(this, std::move(only_child));

        if (was_active)
            only_child_ref->set_active();

        return only_child_ref;
    }
    return nullptr;
}

NodeParent SplitNode::get_or_upgrade_to_parent_node() { return this; }

void SplitNode::for_each_node(const std::function<void(Node)> &f) {
    f(this);
    for (auto &c : children)
        c.node->for_each_node(f);
}

OwnedNode SplitNode::swap_child(Node node, OwnedNode other) {
    auto child = find_child(node);
    if (child == children.end())
        LOGE("Node ", node, " not found in split node: ", this);

    other->parent = this;
    other->set_floating(false);
    other->set_ws(get_ws());
    other->set_geometry(child->node->get_geometry());

    std::swap(child->node, other);

    child->node->notify_initialized();

    return other;
}

void SplitNode::swap_children(Node a, Node b) {
    auto child_a = find_child(a);
    if (child_a == children.end())
        LOGE("Node ", a, " not found in split node: ", this);

    auto child_b = find_child(b);
    if (child_b == children.end())
        LOGE("Node ", b, " not found in split node: ", this);

    std::iter_swap(child_a, child_b);
    refresh_geometry();
}

Node SplitNode::get_last_active_node() {
    if (children.empty())
        return this;

    auto &child = children.at(active_child);
    if (auto split = child.node->as_split_node())
        return split->get_last_active_node();

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
            auto const prev = child[-1].node.get();                            \
            swap_children(child[0].node.get(), prev);                          \
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
            auto const next = child[1].node.get();                             \
            swap_children(child[0].node.get(), next);                          \
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

void SplitNode::set_sublayer(nonstd::observer_ptr<wf::sublayer_t> sublayer) {
    for (auto &child : children)
        child.node->set_sublayer(sublayer);
}

void SplitNode::bring_to_front() {
    const auto ac = empty() ? nullptr : children.at(active_child).node.get();

    for (auto &child : children)
        if (child.node.get() != ac)
            child.node->bring_to_front();

    // Bring the active child in front of the other children.
    if (ac)
        ac->bring_to_front();
}

void SplitNode::set_ws(WorkspaceRef ws) {
    INode::set_ws(ws);

    for (auto &child : children)
        child.node->set_ws(ws);
}

void SplitNode::on_initialized() {
    SplitNodeSignalData data = {};
    data.node = this;
    ws->output->emit_signal("swf-split-node-attached", &data);
}

void SplitNode::set_geometry(const wf::geometry_t geo) {
    geometry = geo;

    if (children.empty()) {
        if (parent->as_split_node() != nullptr)
            LOGE(this, ": Attempt to set geometry of empty split node.");
        return;
    }

    if (pure_set_geo)
        return;

    auto inner = get_inner_geometry();

    switch (split_type) {
    case SplitType::VSPLIT:
    case SplitType::HSPLIT: {
        const uint32_t size =
            split_type == SplitType::VSPLIT ? inner.width : inner.height;

        {
            uint32_t total_children_size = 0;
            for (auto &c : children)
                total_children_size += c.size;

            // Fix improper total child size by resyncing with ratios.
            if (total_children_size != size)
                sync_sizes_to_ratios();
        }

        int offset = 0;
        for (auto &c : children) {
            if (split_type == SplitType::VSPLIT)
                c.node->set_geometry({
                    inner.x + offset,
                    inner.y,
                    (int)c.size,
                    inner.height,
                });
            else
                c.node->set_geometry({
                    inner.x,
                    inner.y + offset,
                    inner.width,
                    (int)c.size,
                });
            offset += (int)c.size;
        }

        assert("Children sizes should add up to total size." &&
               (uint32_t)offset == size);

        break;
    }
    case SplitType::TABBED:
    case SplitType::STACKED: {
        for (auto &child : children)
            child.node->set_geometry(inner);
        break;
    }
    }
}

// Workspace

Workspace::Workspace(wf::point_t wsid, wf::geometry_t geo,
                     nonstd::observer_ptr<Swayfire> plugin)
    : workarea(geo), wsid(wsid), plugin(plugin), output(plugin->output) {

    tiled_root.sublayer = output->workspace->create_sublayer(
        wf::LAYER_WORKSPACE, wf::SUBLAYER_FLOATING);
    (void)swap_tiled_root(std::make_unique<SplitNode>(geo));
    LOGD("ws created with root ", tiled_root.node->to_string());
    active_node = tiled_root.node;

    floating_sublayer = output->workspace->create_sublayer(
        wf::LAYER_WORKSPACE, wf::SUBLAYER_DOCKED_ABOVE);

    output->connect_signal("workarea-changed", &on_workarea_changed);
}

Workspace::~Workspace() {
    output->disconnect_signal(&on_workarea_changed);
    output->workspace->destroy_sublayer(tiled_root.sublayer);
    output->workspace->destroy_sublayer(floating_sublayer);
}

void Workspace::set_active_node(Node node) {
    active_node = node;

    if (!node)
        return;

    node->parent->set_active_child(node);
    node->find_root_parent()->bring_to_front();
    node->on_set_active();
}

Node Workspace::get_active_node() {
    assert(active_node && "There should always be a valid active node set.");
    return active_node;
}

void Workspace::insert_floating_node(OwnedNode node) {
    node->parent = this;
    node->set_floating(true);
    node->set_ws(this);
    node->set_sublayer(floating_sublayer);
    node->notify_initialized();

    floating_nodes.push_back({std::move(node), floating_sublayer});
}

Workspace::FloatingNodeIter Workspace::find_floating(Node node) {
    return std::find_if(floating_nodes.begin(), floating_nodes.end(),
                        [&](auto &c) { return c.node.get() == node.get(); });
}

OwnedNode Workspace::remove_floating_node(Node node, bool reset_active) {
    auto child = find_floating(node);
    if (child == floating_nodes.end()) {
        LOGE("Node not floating in ", this, ": ", node);
        return nullptr;
    }

    auto owned_node = std::move(child->node);

    floating_nodes.erase(child);

    if (floating_nodes.empty())
        active_floating = 0;
    else
        active_floating = std::clamp(active_floating, (uint32_t)0,
                                     (uint32_t)(floating_nodes.size() - 1));

    if (reset_active && node.get() == active_node.get())
        reset_active_node();

    return owned_node;
}

OwnedNode Workspace::swap_floating_node(Node node, OwnedNode other) {
    auto child = find_floating(node);
    if (child == floating_nodes.end()) {
        LOGE("Node not floating in ", this, ": ", node);
        return nullptr;
    }

    other->parent = this;
    other->set_floating(true);
    other->set_ws(this);
    other->set_geometry(child->node->get_geometry());
    other->set_sublayer(child->sublayer);

    // swap the pointers
    std::swap(child->node, other);

    child->node->notify_initialized();
    return other;
}

Node Workspace::get_active_floating_node() {
    if (floating_nodes.empty())
        return nullptr;

    return floating_nodes.at(active_floating).node.get();
}

std::unique_ptr<SplitNode>
Workspace::swap_tiled_root(std::unique_ptr<SplitNode> other) {
    auto ret = std::move(tiled_root.node);

    tiled_root.node = std::move(other);
    tiled_root.node->parent = this;
    tiled_root.node->set_floating(false);
    tiled_root.node->set_ws(this);
    tiled_root.node->set_sublayer(tiled_root.sublayer);
    tiled_root.node->notify_initialized();

    return ret;
}

void Workspace::insert_tiled_node(OwnedNode node) {
    assert(get_active_node().get() != node.get() &&
           "Cannot insert node into itself.");
    auto parent = get_active_node()->get_or_upgrade_to_parent_node();

    parent->insert_child(std::move(node));
}

OwnedNode Workspace::remove_tiled_node(Node node, bool reset_active) {
    if (node->get_floating() || node->get_ws().get() != this) {
        LOGE("Node not tiled in ", this, ": ", node);
        return nullptr;
    }

    auto old_parent = node->parent;
    auto owned_node = node->parent->remove_child(node);

    if (reset_active && node.get() == active_node.get())
        reset_active_node();

    if (old_parent.get() != tiled_root.node.get()) {
        if (auto sparent = old_parent->as_split_node()) {
            if (sparent->empty()) {
                // reset_active = true, since we're possibly destroying the
                // active node here.
                (void)remove_node(sparent, true);
            } else if (sparent->child_at(0)->as_view_node()) {
                sparent->try_downgrade();
            }
        }
    }

    return owned_node;
}

OwnedNode Workspace::remove_node(Node node, bool reset_active) {
    return node->get_floating() ? remove_floating_node(node, reset_active)
                                : remove_tiled_node(node, reset_active);
}

void Workspace::reset_active_node() {
    active_node = tiled_root.node->get_last_active_node();
}

void Workspace::insert_child(OwnedNode node) {
    tiled_root.node->insert_child(std::move(node));
}

OwnedNode Workspace::remove_child(Node node) {
    OwnedNode ret;

    if (node->get_floating()) {
        // floating nodes are always direct children of the workspace
        ret = remove_floating_node(node, false);
    } else if (node.get() == tiled_root.node.get()) {
        ret = swap_tiled_root(std::make_unique<SplitNode>(workarea));
    } else {
        LOGE("Node is not a direct child of ", this, ": ", node);
        return nullptr;
    }

    if (node.get() == active_node.get())
        tiled_root.node->set_active();

    return ret;
}

OwnedNode Workspace::swap_child(Node node, OwnedNode other) {
    if (node->get_floating()) {
        // floating nodes are always direct children of the workspace
        return swap_floating_node(node, std::move(other));
    } else if (node.get() == tiled_root.node.get()) {
        auto other_ = other.release();
        if (auto other_split = dynamic_cast<SplitNode *>(other_)) {
            return swap_tiled_root(std::unique_ptr<SplitNode>(other_split));

        } else {
            LOGE("Cannot swap non-split node with tiled-root node of ", this);

            delete other_;
            return nullptr;
        }
    } else {
        LOGE("Node is not a direct child of ", this, ": ", node);
        return nullptr;
    }
}

void Workspace::swap_children(Node a, Node b) {
    if (a->get_floating() && b->get_floating()) {
        auto tmp = a->get_geometry();
        a->set_geometry(b->get_geometry());
        b->set_geometry(tmp);
    } else {
        if (b->get_floating())
            std::swap(a, b);
        assert(a->get_floating());

        if (b.get() != tiled_root.node.get()) {
            LOGE("Node is not a direct child of ", this, ": ", b);
            return;
        }

        auto old_floating_geo = a->get_geometry();

        auto owned_a = remove_floating_node(a);
        std::unique_ptr<SplitNode> old_root;

        if (auto asplit = owned_a->as_split_node()) {
            old_root = swap_tiled_root(
                std::unique_ptr<SplitNode>((SplitNode *)owned_a.release()));
        } else {
            old_root = std::unique_ptr<SplitNode>(
                (SplitNode *)remove_child(tiled_root.node.get()).release());
            insert_tiled_node(std::move(owned_a));
        }

        auto old_root_ref = old_root.get();
        insert_floating_node(std::move(old_root));

        old_root_ref->set_geometry(old_floating_geo);
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
        assert(
            node.get() == tiled_root.node.get() &&
            "set_active_child should only be called on direct children nodes.");
    }
}

Node Workspace::get_active_child() const {
    return active_node->find_root_parent();
}

void Workspace::set_workarea(wf::geometry_t geo) {
    workarea = geo;
    tiled_root.node->set_geometry(geo);

    for (auto &floating : floating_nodes) {
        auto ngeo = floating.node->get_geometry();

        // Make sure they are still visible in the workarea.

        if (geo.width > MIN_VIEW_SIZE)
            ngeo.x = std::clamp(ngeo.x, MIN_VIEW_SIZE - ngeo.width,
                                geo.width - MIN_VIEW_SIZE);

        if (geo.height > MIN_VIEW_SIZE)
            ngeo.y = std::clamp(ngeo.y, MIN_VIEW_SIZE - ngeo.height,
                                geo.height - MIN_VIEW_SIZE);

        // Refresh their geometry.
        floating.node->set_geometry(ngeo);
    }
}

nonstd::observer_ptr<wf::sublayer_t> Workspace::get_child_sublayer(Node node) {
    if (node.get() == tiled_root.node.get())
        return tiled_root.sublayer;

    auto child = find_floating(node);
    if (child == floating_nodes.end()) {
        LOGE("Node not a direct child of ", this, ": ", node);
        return nullptr;
    }

    return child->sublayer;
}

void Workspace::tile_request(Node const node, const bool tile) {
    const bool was_active = active_node.get() == node.get();

    if (node->get_floating() && tile) {
        insert_tiled_node(remove_floating_node(node));

    } else if (!node->get_floating() && !tile) {
        // Avoid untiling empty tiled root node.
        if (node.get() == tiled_root.node.get() && tiled_root.node->empty())
            return;

        insert_floating_node(remove_tiled_node(node));
        if (active_node.get() == node.get())
            active_floating =
                std::distance(floating_nodes.begin(), find_floating(node));
    }

    // The remove_x_node() calls reset the active node (without actually
    // sending focus requests), so we can just set it back here.
    if (was_active)
        active_node = node;
}

void Workspace::for_each_node(const std::function<void(Node)> &f) {
    for (auto &floating : floating_nodes) {
        floating.node->for_each_node(f);
    }
    tiled_root.node->for_each_node(f);
}

Node Workspace::get_last_active_node() { return active_node; }

Node Workspace::get_adjacent(Node node, Direction dir) {
    if (node.get() == tiled_root.node.get()) {
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
                if (a.node.get() == node.get())                                \
                    return false;                                              \
                if (b.node.get() == node.get())                                \
                    return true;                                               \
                                                                               \
                auto apoint = nonwf::geometry_center(a.node->get_geometry());  \
                auto bpoint = nonwf::geometry_center(b.node->get_geometry());  \
                                                                               \
                if (center.axis cmp apoint.axis)                               \
                    return false;                                              \
                if (center.axis cmp bpoint.axis)                               \
                    return true;                                               \
                                                                               \
                return std::abs(center.axis - apoint.axis) <                   \
                       std::abs(center.axis - bpoint.axis);                    \
            });                                                                \
        if (closest == fl.end() || closest->node.get() == node.get() ||        \
            center                                                             \
                .axis cmp nonwf::geometry_center(                              \
                    closest->node->get_geometry())                             \
                .axis)                                                         \
            return nullptr;                                                    \
        else                                                                   \
            return closest->node.get();                                        \
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
                             nonstd::observer_ptr<Swayfire> plugin) {

    workspaces.resize(ndims.width);

    auto x = 0;
    for (auto &col : workspaces) {
        if (col.size() == (uint32_t)ndims.height) {
            // noop
        } else if (col.size() < (uint32_t)ndims.height) {
            col.reserve(ndims.height);
            for (int32_t y = col.size(); y < ndims.height; y++) {
                col.push_back(std::unique_ptr<Workspace>(
                    new Workspace({x, y}, geo, plugin)));
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
    const auto wsid = output->workspace->get_current_workspace();
    return workspaces.get(wsid);
}

WorkspaceRef Swayfire::get_view_workspace(wayfire_view view,
                                          bool with_transform) {
    assert(view->get_output() == output);
    const auto wsid = nonwf::get_view_workspace(view, with_transform);
    return workspaces.get(wsid);
}

std::unique_ptr<ViewNode> Swayfire::init_view_node(wayfire_view view) {
    auto node = std::make_unique<ViewNode>(view);
    view->store_data<ViewData>(std::make_unique<ViewData>(node));

    LOGD("New view-node for ", view->to_string(), ": ", node.get());
    return node;
}

void Swayfire::bind_signals() {
    output->connect_signal("view-focused", &on_view_focused);
    output->connect_signal("view-fullscreen-request",
                           &on_view_fullscreen_request);
    output->connect_signal("view-tile-request", &on_view_tile_request);
    output->connect_signal("view-layer-attached", &on_view_attached);
    output->connect_signal("view-minimize-request", &on_view_minimized);
}

void Swayfire::unbind_signals() {
    output->disconnect_signal(&on_view_minimized);
    output->disconnect_signal(&on_view_attached);
    output->disconnect_signal(&on_view_tile_request);
    output->disconnect_signal(&on_view_fullscreen_request);
    output->disconnect_signal(&on_view_focused);
}

void Swayfire::init() {
    LOGD("==== init ====");
    output->workspace->set_workspace_implementation(
        std::make_unique<SwayfireWorkspaceImpl>(), true);

    auto grid_dims = output->workspace->get_workspace_grid_size();

    workspaces.update_dims(grid_dims, output->workspace->get_workarea(), this);

    auto views = output->workspace->get_views_in_layer(wf::ALL_LAYERS);

    for (auto view : views) {
        if (view->role == wf::VIEW_ROLE_TOPLEVEL) {
            auto ws = workspaces.get(nonwf::get_view_workspace(view));
            ws->insert_tiled_node(init_view_node(view));
        }
    }

    if (auto active_view = output->get_active_view())
        if (auto node = get_view_node(active_view))
            node->set_active();

    init_grab_interface();

    bind_signals();
    bind_keys();

    output->store_data(std::make_unique<SwayfireCustomData>(this),
                       "swayfire-core");

    output->emit_signal("swf-init", nullptr);
}

void Swayfire::fini() {
    output->emit_signal("swf-fini", nullptr);

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
