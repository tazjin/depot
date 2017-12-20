# Gemma

Gemma is a simple application to track *recurring* tasks, named after Gemma
Hartley who [wrote an article][] about task distribution issues in households.

## Background

(Skip this if you just want the technical bits)

Gemma's article launched a discussion in my friend circle about what causes an
uneven distribution of household workload. I theorised that this is not so much
a gender issue, but mostly a discoverability issue.

Usually one person in a household is aware of what needs to be done, but in many
cases the "overhead" of delegating the tasks would actually take more time than
simply doing the task.

I theorise further that the person (or people) who do a smaller share of the
household work would often do the work if they had a convenient way to become
aware of what needs to be done. Many times the "household manager" has the
function of tracking non-obvious tasks like when bedsheets were last changed -
shouldn't it be possible to actually distribute this information somehow?

## The Project

This project is an initial attempt at sketching out a little application that
aids with reminding users of recurring tasks. Some basic ideas:

* The system should be blame-free.
* There should be as little usage overhead as possible so that people actually
  do use it.
* It should work mostly passively without much user interaction.

I believe that the basic (*very* simple) idea behind Gemma solves these issues.
Unfortunately my previous relationship fell apart before I actually got to test
this out in a real-life situation involving multiple people, but feedback from
other potential test subjects would be welcome! :)

## Overview

Gemma is a Common Lisp application in which a list of recurring tasks is
declared, together with the *maximum interval* at which they should be completed
(in days). Example:

```lisp
;; Bathroom tasks
(deftask bathroom/wipe-mirror 7)
(deftask bathroom/wipe-counter 7)

;; Bedroom tasks
(deftask bedroom/change-sheets 7)
(deftask bedroom/vacuum 10)

;; Kitchen tasks
(deftask kitchen/trash 3)
(deftask kitchen/wipe-counters 3)
(deftask kitchen/vacuum 5 "Kitchen has more crumbs and such!")

;; Entire place
(deftask clean-windows 60)
```

These tasks are marked with their last completion time and tracked by Gemma. A
simple Elm-based frontend application displays the tasks sorted by their
"urgency" and features a button to mark a task as completed:

![Gemma screenshot](http://i.imgur.com/n7FFMJH.png)

Marking a task as completed resets its counter and moves it to the bottom of the
task list.

In theory this *should be it*, the frontend is made available to household
members in some easily accessible place (e.g. an old phone glued to the fridge!)
and people should attempt to develop a habit of checking what needs to be done
occasionally.

The "household manager" still exists as a role of the household because someone
is entering the tasks into the application, but if my theory about people not
actually being actively *unwilling* to do tasks is correct this could help a
lot.

## Usage

(*Note*: Gemma is alpha software so the below is clearly not the final goal)

Right now using this is non-trivial, but I'll eventually make a better
distribution. Basically you need to know Common Lisp (in which case you'll know
how to get the backend running) and have `elm-reactor` installed to run the
development version of the frontend application.

[wrote an article]: http://www.harpersbazaar.com/culture/features/a12063822/emotional-labor-gender-equality/
