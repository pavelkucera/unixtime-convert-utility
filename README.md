# Unixtime Utility

This is an experimental project where I'm trying to improve my Haskell skills. I am aware of a few issues and if you have any suggestions on improvements, feel free to share them!

## Why

At work we store some time recors as unix timestamps (due to historical reasons) and when I see a value such as `1356120720` in the database, it usually doesn't hit me that it translates to 2012-12-21. Which is why I need a tool which converts a unixtimestamp to

1) local time - very useful for recent time records
2) PST - half of the company resides on the West Coast…
