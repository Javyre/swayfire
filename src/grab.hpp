#ifndef GRAB_HPP
#define GRAB_HPP

#include <bits/stdint-uintn.h>
#include <swayfire.hpp>

// RAII gesture controller
class active_grab_t {
    protected:
        nonstd::observer_ptr<swayfire_t> plugin;

        static std::unique_ptr<active_grab_t> try_activate(
                nonstd::observer_ptr<swayfire_t> plugin,
                std::function<std::unique_ptr<active_grab_t>()> cons);

    public:
        active_grab_t(active_grab_t &) = delete;
        active_grab_t(active_grab_t &&) = default;
        active_grab_t &operator=(active_grab_t &&) = default;

        virtual void pointer_motion(uint32_t, uint32_t) {}
        virtual void button(uint32_t, uint32_t) {}

        active_grab_t( nonstd::observer_ptr<swayfire_t> plugin) : plugin(plugin) {};
        virtual ~active_grab_t();
};

class active_button_drag_t : public active_grab_t {
    private:
        uint32_t deactivate_button;

    public:
        void button(uint32_t, uint32_t) override;

        active_button_drag_t(
                nonstd::observer_ptr<swayfire_t> plugin, 
                wf::buttonbinding_t deactivate_butt) :
            active_grab_t(plugin),
            deactivate_button(wf::buttonbinding_t(deactivate_butt).get_button()) {};
};

class active_move_t : public active_button_drag_t {
    private:
        node_t dragged;
        wf::geometry_t original_geo;
        wf::point_t pointer_start;

    public:
        active_move_t(nonstd::observer_ptr<swayfire_t> plugin,
                wf::buttonbinding_t deactivate_butt) :
            active_button_drag_t(plugin, deactivate_butt) {}

        void pointer_motion(uint32_t x, uint32_t y) override;

        static std::unique_ptr<active_grab_t> construct(
                nonstd::observer_ptr<swayfire_t> plugin,
                node_t dragged);
};

class active_resize_t : public active_button_drag_t {
    private:
        node_t dragged;
        wf::geometry_t original_geo;
        wf::point_t pointer_start;
        uint8_t locked_edges;

    public:
        active_resize_t(nonstd::observer_ptr<swayfire_t> plugin,
                wf::buttonbinding_t deactivate_butt) :
            active_button_drag_t(plugin, deactivate_butt) {}

        void pointer_motion(uint32_t x, uint32_t y) override;

        static std::unique_ptr<active_grab_t> construct(
                nonstd::observer_ptr<swayfire_t> plugin,
                node_t dragged);
};

#endif // ifndef GRAB_HPP
