#ifndef GRAB_HPP
#define GRAB_HPP

#include <bits/stdint-uintn.h>
#include <swayfire.hpp>

// RAII gesture controller
class IActiveGrab {
  protected:
    nonstd::observer_ptr<Swayfire> plugin;

    static std::unique_ptr<IActiveGrab>
    try_activate(nonstd::observer_ptr<Swayfire> plugin,
                 const std::function<std::unique_ptr<IActiveGrab>()> &cons);

  public:
    IActiveGrab(IActiveGrab &) = delete;
    IActiveGrab(IActiveGrab &&) = default;
    IActiveGrab &operator=(IActiveGrab &&) = default;

    virtual void pointer_motion(uint32_t, uint32_t) {}
    virtual void button(uint32_t, uint32_t) {}

    IActiveGrab(nonstd::observer_ptr<Swayfire> plugin) : plugin(plugin){};
    virtual ~IActiveGrab();
};

class IActiveButtonDrag : public IActiveGrab {
  private:
    uint32_t deactivate_button;

  public:
    void button(uint32_t, uint32_t) override;

    IActiveButtonDrag(nonstd::observer_ptr<Swayfire> plugin,
                      wf::buttonbinding_t deactivate_butt)
        : IActiveGrab(plugin),
          deactivate_button(
              wf::buttonbinding_t(deactivate_butt).get_button()){};
};

class ActiveMove : public IActiveButtonDrag {
  private:
    Node dragged;
    wf::geometry_t original_geo;
    wf::point_t pointer_start;

  public:
    ActiveMove(nonstd::observer_ptr<Swayfire> plugin,
               wf::buttonbinding_t deactivate_butt)
        : IActiveButtonDrag(plugin, deactivate_butt) {}

    void pointer_motion(uint32_t x, uint32_t y) override;

    static std::unique_ptr<IActiveGrab>
    construct(nonstd::observer_ptr<Swayfire> plugin, Node dragged);
};

class ActiveResize : public IActiveButtonDrag {
  private:
    Node dragged;
    wf::geometry_t original_geo;
    wf::point_t pointer_start;
    uint8_t locked_edges;

  public:
    ActiveResize(nonstd::observer_ptr<Swayfire> plugin,
                 wf::buttonbinding_t deactivate_butt)
        : IActiveButtonDrag(plugin, deactivate_butt) {}

    void pointer_motion(uint32_t x, uint32_t y) override;

    static std::unique_ptr<IActiveGrab>
    construct(nonstd::observer_ptr<Swayfire> plugin, Node dragged);
};

#endif // ifndef GRAB_HPP
