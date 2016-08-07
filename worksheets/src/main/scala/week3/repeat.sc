def REPEAT(command: => Unit)(condition: => Boolean): Unit = {
  command
  if (!condition) {
    REPEAT(command)(condition)
  }
}

