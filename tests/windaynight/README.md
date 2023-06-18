# WinDayNight
### Windows theme toggler

A little tray app that toggles Windows' theme.
It can be set to run at startup.
It sets both the System and App theme.

When using it from the command line, a script, or Windows task scheduler, you can pass `toggle`, `light` or `dark` as an argument to control its behaviour (no argument will launch the tray app).

How to build:
- Make sure you have MSVC (other toolchains not tested) and Windows SDK
- run `build.cmd`
