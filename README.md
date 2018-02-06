Utility to generate [TaskPaper](https://www.taskpaper.com/) files from a Things
sqlite file.

This is a work in progress -- probably not ready for primetime.

This program takes a Things3 sqlite database, and converts it into a "similar"
TaskPaper structure. I'm being a bit opinionated about how this looks, since
the models are not 1:1, but I am comfortable with my project/area/inbox and
unfiled/someday concepts from Things, so they are replicated here.

You can see what's working now from the Todo list below. Sorry, not much
documentation, and it's a scala program.

If you're brave enough to try, install sbt and run "sbt run" from the shell. If
everything works, you'll end up with a things.taskpaper in the same directory
as this program.

That should more or less work in TaskPaper.

I'm working on this in anger to get my current workflows working in TaskPaper.
As much as I love Things, it is somehow too hard to get the data out, and too
hard to collaborate with others.

Once I'm done here, I hope to write up more about my workflow, and also a
cronjob that can make recurring tasks work.

### Todo

 - [x] Tasks - parsing
 - [x] Completed Tasks
 - [x] Areas - parsing
 - [x] Project - parsing
 - [x] Notes formatting
 - [x] Headings - parsing
 - [x] Ordering
 - [x] Tasks - printing
 - [x] Headings - printing
 - [x] Areas - printing
 - [x] Projects - printing
 - [x] Due date support
 - [x] Inbox
 - [ ] Repeating
 - [x] Checklists



