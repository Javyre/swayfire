#ifndef SWAYFIRE_GRAB_HPP
#define SWAYFIRE_GRAB_HPP

#include "core.hpp"

/// RAII gesture controller interface.
class IActiveGrab {
  protected:
    /// Reference to swayfire.
    nonstd::observer_ptr<Swayfire> plugin;

    /// Try to activate the grab interface and call the callback if successful.
    static std::unique_ptr<IActiveGrab>
    try_activate(nonstd::observer_ptr<Swayfire> plugin,
                 const std::function<std::unique_ptr<IActiveGrab>()> &cons);

  public:
    IActiveGrab(nonstd::observer_ptr<Swayfire> plugin) : plugin(plugin){};
    IActiveGrab(IActiveGrab &) = delete;
    IActiveGrab(IActiveGrab &&) = default;
    IActiveGrab &operator=(IActiveGrab &&) = default;

    /// Handle the pointer motion event.
    virtual void pointer_motion(uint32_t, uint32_t) {}

    /// Handle the button event.
    virtual void button(uint32_t, uint32_t) {}

    /// Destroy the active grab and disable the grab interface.
    virtual ~IActiveGrab();
};

/// RAII button drag gesture controller interface.
class IActiveButtonDrag : public IActiveGrab {
  private:
    /// The button that must be unpressed to deactivate the gesture.
    uint32_t deactivate_button;

  public:
    void button(uint32_t, uint32_t) override;

    IActiveButtonDrag(nonstd::observer_ptr<Swayfire> plugin,
                      wf::buttonbinding_t deactivate_butt)
        : IActiveGrab(plugin),
          deactivate_button(
              wf::buttonbinding_t(deactivate_butt).get_button()){};
};

/// Button drag view move gesture.
class ActiveMove final : public IActiveButtonDrag {
  private:
    /// The node being dragged.
    Node dragged;

    /// The original outer geometry of the dragged node.
    wf::geometry_t original_geo;

    /// The initial position of the pointer.
    wf::point_t pointer_start;

  public:
    ActiveMove(nonstd::observer_ptr<Swayfire> plugin,
               wf::buttonbinding_t deactivate_butt)
        : IActiveButtonDrag(plugin, deactivate_butt) {}

    void pointer_motion(uint32_t x, uint32_t y) override;

    /// Try to activate the grab_interface and begin an move gesture.
    static std::unique_ptr<IActiveGrab>
    construct(nonstd::observer_ptr<Swayfire> plugin, Node dragged);
};

/// Button drag view resize gesture.
class ActiveResize final : public IActiveButtonDrag {
  private:
    /// The node being resized.
    Node dragged;

    /// The root parent of the dragged node that isn't the workspace or just the
    /// node itself if it is a direct child of the workspace.
    Node root_node;

    /// The original outer geometry of the resizing node.
    wf::geometry_t original_geo;

    /// The initial position of the pointer.
    wf::point_t pointer_start;

    /// The moving edges of the resizing node.
    uint8_t resizing_edges;

  public:
    ActiveResize(nonstd::observer_ptr<Swayfire> plugin,
                 wf::buttonbinding_t deactivate_butt)
        : IActiveButtonDrag(plugin, deactivate_butt) {}

    ~ActiveResize() override;

    void pointer_motion(uint32_t x, uint32_t y) override;

    /// Try to activate the grab_interface and begin an resize gesture.
    static std::unique_ptr<IActiveGrab>
    construct(nonstd::observer_ptr<Swayfire> plugin, Node dragged);
};

#endif // ifndef SWAYFIRE_GRAB_HPP
