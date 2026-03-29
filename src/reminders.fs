module Reminders

open System
open Lit

type DayReminder =
    { Day: string
      Emoji: string
      Items: string list }

type WeeklyReminder =
    { WeekOf: string
      Highlights: string list
      DailyReminders: DayReminder list
      EasterHomework: string list
      KeyDates: string list }

let currentWeekReminders =
    { WeekOf = "Mar 23"
      Highlights =
          [ "End of Term - Fri Mar 27 (midday) - pick up TBD"
            "EDI Coffee Morning - Thu Mar 26, 8:15-9:00 (Miss Vanessa's classroom)"
            "Last day of term class party likely Friday - food list to follow"
            "Read the weekly newsletter" ]
      DailyReminders =
          [ { Day = "Monday"
              Emoji = "&#x1F3C9;"
              Items =
                  [ "Spelling, Comprehension, Grammar, Paper maths" ] }
            { Day = "Tuesday"
              Emoji = "&#x1F3C9;"
              Items =
                  [ "VR, Online maths"
                    "Maths booster: 8:30-9:00am" ] }
            { Day = "Wednesday"
              Emoji = "&#x1F393;"
              Items = [] }
            { Day = "Thursday"
              Emoji = "&#x1F393;"
              Items =
                  [ "NVR"
                    "English booster: 8:30-9:00am"
                    "EDI Coffee Morning: 8:15-9:00 (Vanessa's classroom)" ] }
            { Day = "Friday"
              Emoji = "&#x1F3C9;"
              Items =
                  [ "Easter Bonnet assembly (whole school) - midday finish - pick up TBD" ] } ]
      EasterHomework =
          [ "Lots of homework/tests etc. - see Gina G's email last week" ]
      KeyDates =
          [ "First day back - Wed Apr 22"
            "Parent Football & Netball Competition - Fri Apr 24, 5:30-7:00pm (Wimbledon High School Playing Fields)"
            "Heritage Day - Tue May 12 (whole school)" ] }

let private renderListItems items =
    items
    |> List.map (fun item ->
        html $"""<li class="mb-1">{item}</li>""")

let private renderDayReminder (day: DayReminder) =
    let items =
        match day.Items with
        | [] -> html $"""<span class="text-stone-500 text-sm italic">No specific tasks</span>"""
        | items ->
            html $"""<ul class="list-disc list-inside text-sm ml-2">{renderListItems items}</ul>"""

    html
        $"""
        <div class="mb-2">
            <div class="font-semibold">{day.Emoji} {day.Day}</div>
            {items}
        </div>
    """

let remindersText =
    let r = currentWeekReminders

    let todayName =
        DateTime.Now.DayOfWeek.ToString()

    html
        $"""
        <div class="modal-body p-3 text-slate-800 text-sm">
            <div class="mb-3">
                <h6 class="font-bold text-base mb-1">&#x1F4C5; Weekly Reminders - Week of {r.WeekOf}</h6>
                <ul class="list-disc list-inside ml-1">
                    {renderListItems r.Highlights}
                </ul>
            </div>

            <hr class="my-2 border-stone-500"/>

            <div class="mb-3">
                <h6 class="font-bold text-base mb-1">Daily Reminders</h6>
                <p class="text-xs text-stone-600 mb-2">Today is {todayName}</p>
                {r.DailyReminders |> List.map renderDayReminder}
            </div>

            <hr class="my-2 border-stone-500"/>

            <div class="mb-3">
                <h6 class="font-bold text-base mb-1">&#x1F4DD; Easter Homework</h6>
                <ul class="list-disc list-inside ml-1">
                    {renderListItems r.EasterHomework}
                </ul>
            </div>

            <hr class="my-2 border-stone-500"/>

            <div class="mb-1">
                <h6 class="font-bold text-base mb-1">&#x1F4C5; Key Dates</h6>
                <ul class="list-disc list-inside ml-1">
                    {renderListItems r.KeyDates}
                </ul>
            </div>
        </div>
    """
