#include "swayfire.hpp"
#include <wayfire/geometry.hpp>

// INode

wf::dimensions_t INode::try_resize(wf::dimensions_t ndims, uint32_t edges) {
    return parent->try_resize_child(this, ndims, edges);
}

// SplitNode

int32_t SplitNode::try_move_front_edge(SplitChildIter child, int32_t delta,
                                       bool use_preferred_sizes) {
    return try_move_edge(child, delta, true, use_preferred_sizes);
}

int32_t SplitNode::try_move_back_edge(SplitChildIter child, int32_t delta,
                                      bool use_preferred_sizes) {
    return try_move_edge(child, delta, false, use_preferred_sizes);
}

int32_t SplitNode::try_move_edge(SplitChildIter child, int32_t delta,
                                 bool front, bool use_prefered_sizes) {
    switch (split_type) {
    case SplitType::TABBED:
    case SplitType::STACKED:
        return 0;
    case SplitType::VSPLIT:
    case SplitType::HSPLIT: {
        if ((front && child < children.begin()) ||
            (!front && child > children.end() - 1)) {
            LOGE("Child out of bounds!");
            return 0;
        }
        break;
    }
    }

    int32_t size, pref_size;
    auto geo = get_geometry();
    if (split_type == SplitType::VSPLIT)
        size = geo.width;
    else
        size = geo.height;

    if (use_prefered_sizes) {
        if (split_type == SplitType::VSPLIT)
            pref_size = preferred_size.value().width;
        else
            pref_size = preferred_size.value().height;
    }

    if (front && child == children.begin()) {
        int32_t delta_size = -delta;
        if (use_prefered_sizes)
            delta_size = std::max(size + delta_size, pref_size) - size;

        int32_t new_size =
            split_type == SplitType::VSPLIT
                ? parent
                      ->try_resize_child(this, {size + delta_size, geo.height},
                                         WLR_EDGE_LEFT)
                      .width
                : parent
                      ->try_resize_child(this, {geo.width, size + delta_size},
                                         WLR_EDGE_TOP)
                      .height;

        float scale = (float)size / (float)new_size;

        float total_ratio = 0;
        child++;
        for (; child != children.end(); child++) {
            child->ratio *= scale;
            total_ratio += child->ratio;
        }
        children.front().ratio = 1.0f - total_ratio;

        return -(new_size - size);
    } else if (!front && child == children.end() - 1) {
        int32_t delta_size = delta;
        if (use_prefered_sizes)
            delta_size = std::max(size + delta_size, pref_size) - size;

        int32_t new_size =
            split_type == SplitType::VSPLIT
                ? parent
                      ->try_resize_child(this, {size + delta_size, geo.height},
                                         WLR_EDGE_RIGHT)
                      .width
                : parent
                      ->try_resize_child(this, {geo.width, size + delta_size},
                                         WLR_EDGE_BOTTOM)
                      .height;

        float scale = (float)size / (float)new_size;

        float total_ratio = 0;
        auto rchild = std::reverse_iterator(child + 1);
        rchild++;
        for (; rchild != children.rend(); rchild++) {
            rchild->ratio *= scale;
            total_ratio += rchild->ratio;
        }
        children.back().ratio = 1.0f - total_ratio;

        return new_size - size;
    } else {
        auto other = front ? child - 1 : child + 1;

        int32_t total_size = size;
        int32_t child_size, other_size;
        if (split_type == SplitType::VSPLIT) {
            child_size = child->node->get_geometry().width;
            other_size = other->node->get_geometry().width;
        } else {
            child_size = child->node->get_geometry().height;
            other_size = other->node->get_geometry().height;
        }

        int32_t max_nother_size = other_size + child_size - MIN_VIEW_SIZE;

        if (use_prefered_sizes) {
            auto pref = split_type == SplitType::VSPLIT
                            ? other->node->preferred_size.value().width
                            : other->node->preferred_size.value().height;
            max_nother_size = std::min(max_nother_size, (int32_t)(pref));
        }

        int32_t nother_size =
            std::clamp(front ? other_size + delta : other_size - delta,
                       MIN_VIEW_SIZE, max_nother_size);

        float nother_ratio = (float)nother_size / (float)total_size;

        child->ratio += other->ratio - nother_ratio;
        other->ratio = nother_ratio;

        refresh_geometry();

        int32_t amount_moved =
            front ? nother_size - other_size : other_size - nother_size;

        return amount_moved;
    }
}

wf::dimensions_t SplitNode::try_resize_child(Node node, wf::dimensions_t ndims,
                                             uint32_t edges) {
    // Forward iterator starting at child.
    auto child = find_child(node);
    if (child == children.end())
        LOGE("Node ", node, " not found in split node: ", this);
    // Reverse iterator starting at child.
    auto rchild = std::reverse_iterator(child + 1);

    auto child_geo = child->node->get_geometry();

    ndims = nonwf::max(ndims, {MIN_VIEW_SIZE, MIN_VIEW_SIZE});

    wf::dimensions_t delta = {
        ndims.width - child_geo.width,
        ndims.height - child_geo.height,
    };

    if ((edges & (WLR_EDGE_LEFT | WLR_EDGE_RIGHT)) == 0)
        delta.width = 0;
    if ((edges & (WLR_EDGE_TOP | WLR_EDGE_BOTTOM)) == 0)
        delta.height = 0;

    if (delta.width == 0 && delta.height == 0)
        return wf::dimensions(child_geo);

    switch (split_type) {
    case SplitType::VSPLIT:
    case SplitType::HSPLIT: {
        auto delta_size =
            split_type == SplitType::VSPLIT ? delta.width : delta.height;

        if ((split_type == SplitType::VSPLIT && (edges & WLR_EDGE_LEFT)) ||
            (split_type == SplitType::HSPLIT && (edges & WLR_EDGE_TOP))) {
            // Shrinking child from left means moving left edge(s) towards +x.
            auto edge_delta = delta_size * -1;
            if (delta_size > 0) {
                // Shrink siblings starting with nearest
                for (; rchild != children.rend() && edge_delta < 0; rchild++)
                    edge_delta -=
                        try_move_front_edge((rchild + 1).base(), edge_delta);

            } else {
                // Grow siblings starting with furthest
                auto front = children.begin();
                for (; front != child && edge_delta > 0; front++)
                    edge_delta -= try_move_front_edge(front, edge_delta, true);

                if (front == child && edge_delta > 0)
                    edge_delta -= try_move_front_edge(front, edge_delta);
            }
            delta_size = edge_delta * -1;
        } else {
            auto edge_delta = delta_size;
            if (delta_size > 0) {
                // Shrink siblings starting with nearest
                for (; child != children.end() && edge_delta > 0; child++)
                    edge_delta -= try_move_back_edge(child, edge_delta);
            } else {
                // Grow siblings starting with furthest
                auto back = children.rbegin();
                for (; back != rchild && edge_delta < 0; back++)
                    edge_delta -=
                        try_move_back_edge((back + 1).base(), edge_delta, true);

                if (back == rchild && edge_delta < 0)
                    edge_delta -=
                        try_move_back_edge((back + 1).base(), edge_delta);
            }
            delta_size = edge_delta;
        }

        wf::dimensions_t nndims = wf::dimensions(get_geometry());
        if (split_type == SplitType::VSPLIT)
            nndims.height += delta.height;
        else
            nndims.width += delta.width;

        parent->try_resize_child(this, nndims, edges);

        return wf::dimensions(node->get_geometry());
    }
    case SplitType::TABBED:
    case SplitType::STACKED: {
        parent->try_resize_child(this, ndims, edges);
        return wf::dimensions(node->get_geometry());
    }
    }
}

// Workspace

wf::dimensions_t Workspace::try_resize_child(Node child, wf::dimensions_t ndims,
                                             uint32_t edges) {
    if (child->get_floating()) {
        auto ngeo = child->get_geometry();

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

        child->set_geometry(ngeo);
        return wf::dimensions(ngeo);
    }
    return ndims;
}
void SplitNode::begin_resize() {
    INode::begin_resize();
    for (auto &child : children)
        child.node->begin_resize();
}

void SplitNode::end_resize() {
    INode::end_resize();
    for (auto &child : children)
        child.node->end_resize();
}
