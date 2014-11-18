The Call Game Engine
====================

Note
------
* You have to compile sources with `-threaded` due to call's concurrency.

Comparison
------

| Feature        | free-game     | call                     |
| -------------- | ------------- | ------------------------ |
| Policy         | All in one    | Separate as possible     |
| Time           | Frame-based   | Continuous               |
| Window Refresh | Monadic       | Callback                 |
| 2D drawing     | Monadic       | Monoidal                 |
| 3D drawing     |               | Monoidal                 |
| Text           | Monadic       | Objective                |
| Audio          |               | Buffer-wise callback     |
| Keyboard       | Monadic       | Callback, Monadic        |
| Mouse          | Monadic       | Callback, Monadic        |
| Gamepad        |               | Callback, Monadic        |
