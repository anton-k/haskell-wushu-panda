# pomodoro timer

![Pomodoro pic](https://github.com/anton-k/haskell-wushu-panda/blob/main/img/pomodoro-2.png)

Simple timer to help you work in short well balanced periods.
We can install it with stack. Navigate to the project and run `stack install`.
After that we have CLI application:

```
> pomodoro --help
pomodoro timer executes command after so many minutes

Usage: pomodoro [-m|--minutes INT] [-c|--cmd SHELL-COMMAND]
  Pomodoro timer

Available options:
  -m,--minutes INT         How many minutes to wait (default: 25)
  -c,--cmd SHELL-COMMAND   Command to execute on exit (default: "echo \"done\"")
  -h,--help                Show this help text
```

We can create a timer that runs for so many minutes 
shows progress bar and executes a command after it's done:

```
> pomodoro -m 40 -c "cplay bingo.wav"
```

This way we can get notified that time for task is over and have a break.
By default the command prints `done` to console.


