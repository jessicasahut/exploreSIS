## ui.R ##

dashboardPage(
  dashboardHeader(
    title = "explore SIS"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Productivity", 
        tabName = "productivity", 
        icon = icon("line-chart")
      ),
      menuItem(
        "Support Needs", 
        tabName = "support", 
        icon = icon("life-ring")
      ),
      menuItem(
        "Protection", 
        tabName = "protection", 
        icon = icon("shield")
      ),
      menuItem(
        "Medical/Behavioral", 
        tabName = "med-beh", 
        icon = icon("medkit")
      ),
      menuItem(
        "Compare Raters", 
        tabName = "inter_rater", 
        icon = icon("chain-broken")
      ),
      menuItem(
        "Use in Planning", 
        tabName = "planning", 
        icon = icon("paper-plane")
      ),
      selectInput(
        "agency",
        label = "Pick an agency:",
        choices = c("All", levels(unique(scrub_sis$agency))), 
        selected = "All"
      ),
      dateRangeInput(
        'dateRange',
        label = 'Date range:',
        start = begin, 
        end = max(as.Date(scrub_sis$sis_date)[as.Date(scrub_sis$sis_date) <= Sys.Date()])
      ),
      textOutput("valid_date"),
      br(),
      em(
        paste0("   Data updated ",max(as.Date(scrub_sis$sis_date)[as.Date(scrub_sis$sis_date) <= Sys.Date()]))
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "productivity",
        fluidRow(
          column(
            width = 6,
            valueBoxOutput(
              "rate",
              width = NULL
            ), 
            valueBoxOutput(
              "complete",
              width = NULL
            ), 
            valueBoxOutput(
              "needperwk",
              width = NULL
            ),
            box(
              title = "About these indicators", 
              status = "warning",
              collapsible = TRUE, 
              collapsed = TRUE,
              width = NULL,
              tabBox(
                width = NULL,
                tabPanel(
                  "Completion Rate",
                  p("This basically answers the question", 
                    em("Am I on track?"), 
                    "  It's a comparison of 2 percentages: ",
                    br(),
                    strong("Numerator: % of assessments completed"),
                    "This calculates the number of unique clients interviewed in 
                    the selected date range as a percentage of the total clients 
                    currently meeting criteria for assessment", 
                    em("(from QI file)"),
                    br(),
                    strong("Denominator: % of time elapsed"),
                    "This calculates the number of days between the first and last 
                    dates selected in the date range filter as a percentage of the 
                    number of days between the first date in the date range filter 
                    and the due date for initial SIS assessments."
                  ), 
                  p(
                    "So, for example, a score of 100% means that you're completing 
                    assessments at the rate needed to meet the goal, and a score 
                    of 200% would mean that you're completing assessments twice as 
                    quickly as needed to meet your goal, and so on..."
                  )
                  ),
                tabPanel(
                  "% Complete",
                  p(
                    "This indicator calculates the number of unique clients 
                    interviewed in the selected date range as a percentage of the 
                    total clients currently meeting criteria for assessment",
                    em("(from QI file)")),
                  p(
                    em("P.S.  This is exactly the same as the numerator of the"), 
                    strong("Completion Rate")
                  )
                ),
                tabPanel(
                  "Assessments Completed:Needed",
                  p(
                    "This indicator shows us two numbers, calculated as a weekly 
                    value to help us get a sense of the work that is needed each 
                    week:",
                    br(),
                    strong("Assessments Completed: "),
                    "The average number of assessments completed during each week 
                    in the selected date range.",
                    em(
                      "(Note that date ranges including partial weeks will only 
                    count the assessments on the selected dates, which will 
                    likely decrease the average due to incomplete data for 
                    certain weeks.  To avoid this, choose date ranges starting 
                    with a Monday and ending on a Sunday."
                    ),
                    br(),
                    strong("Assessments Needed"),
                    "For this we need to get the total number of people needing 
                    assessments.  For this we take the number of total clients 
                    meeting criteria for assessment", em("(from QI file)"), 
                      "and subtract the number of people who have had assessments 
                    prior to the initial date in the date range selected.  We then 
                    divide the number of people needing assessments by the number 
                    of weeks between the start of the selected date range and the 
                    due date."
                  )
                )
              )
            )
          ),
          column(
            width = 6,
            box(
              title = "On Track?", 
              status = "warning",
              collapsible = TRUE, 
              width = NULL,
              tabBox(
                width = NULL,
                tabPanel(
                  "Chart", 
                  dygraphOutput("on_track")
                ),
                tabPanel(
                  "Table", 
                  dataTableOutput("num_dt")
                ),
                tabPanel(
                  "What if...?",
                  p(
                    "Use the sliders below to understand the potential impact 
                    that various changes might have on interviewer productivity:"
                  ),
                  strong("What if..."),
                  uiOutput("what_staff"),
                  uiOutput("what_prod"),
                  dygraphOutput("on_track_what_if")
                ),
                tabPanel(
                  "About",
                  tabBox(
                    width = NULL,
                    tabPanel(
                      "Chart",
                      br(),
                      strong("On track to what?"),
                      p(
                        "When the SIS was selected by the state for implementation 
                        in 2014, an expectation was set that all individuals meeting 
                        criteria for assessment would be assessed within 3 years."
                      ),
                      p(
                        "Agencies implementing the SIS have been wondering how much 
                        person-time to devote to SIS assessments.  Should this be a 
                        full time person? Part time?  How many assessors do we need 
                        to cover the region on an ongoing basis?"
                      ),
                      strong("Projected Timeline"), 
                      p(
                        "The chart here shows the cumulative number of SIS 
                        assessments completed per week.  The green line shows actual 
                        historical data, while the dotted lines show two potential 
                        futures:",
                        br(),
                        "* What will happen if the region's historical SIS 
                        completion rate continues as it has for the past 3 months?",
                        br(),
                        "* What will need to happen in order to meet the required 
                        timeframe for completion?"
                      )
                    ),
                    tabPanel(
                      "Table",
                      br(),
                      strong("Assessments per Interviewer"),
                      p(
                        "In this table, you can see the total number of SIS 
                        assessments for each interviewer.  Only current interviewers 
                        are displayed."
                      ),
                      p(
                        "To define the interviewer for a given assessment, we use 
                        the field recommended by AAIDD for inter-rater reliability 
                        work.  This field may occasionally ascribe some assessments 
                        incorrectly due to errors in data entry."
                      ),
                      p(
                        "The average number of assessments per interviewer is",
                        round(scrub_sis %>% filter(current_int == T) %>%
                                group_by(interviewer, agency) %>%
                                summarize(n = n()) %>% ungroup() %>% 
                                summarize(avg = mean(n, na.rm = T)), 
                              digits = 1), 
                        ". Please remember that each interviewer has been completing 
                        assessments for various lengths of time and each have a 
                        different proportion of their position designated to 
                        completing SIS assessments."
                      ),
                      p(
                        "Clicking on the green 'plus' signs next to each row allows 
                        you to see the values for columns that don't fit in the 
                        view.  To change which columns are visible, click 'Show/Hide 
                        Columns' in the upper right corner."
                      ),
                      strong("Duration"),
                      p(
                        "In order to estimate how many people hours need to be 
                        dedicated to complete SIS assessments for eligible clients, 
                        we need to get a sense of how long it takes to do a SIS 
                        assessment."
                      ),  
                      p(
                        "While there are some issues which still need to be resolved 
                        with the data, assessments across the region have taken an 
                        average of ",
                        round(mean(scrub_sis$duration, na.rm = T), digits = 1), 
                        " minutes (median = ", 
                        round(median(scrub_sis$duration, na.rm = T), digits = 1), 
                        "). Adding some standard assumptions for travel time and 
                        documentation should allow for a basic estimate of the time 
                        required to complete a single SIS assessment.  From there, 
                        we can estimate the total number of hours which will need to 
                        be devoted to assessment in the region and compare that to 
                        the available hours of existing SIS interviewers in the 
                        region during the time remaining."
                      )
                    ),
                    tabPanel(
                      "What if...?",
                      br(),
                      h4("Sliders"),
                      p(
                        "You can use the sliding bars to look at what might 
                        happen using various scenarios where the number of 
                        interviewers or the productivity of those interviewers 
                        are either decreased or increased.  The first sliding 
                        bar starts at zero and lets you add or substract 
                        interviewers. The second starts at the current average 
                        per week and lets you change to half or 1.5 times that 
                        amount."
                      ),
                      h4("The Fine Print"),
                      p(
                        "The projections here make several assumptions in their 
                        calculations, which are helpful to understand:",
                        br(),
                        strong("Scheduling:"),
                        "It is possible that SIS interviews are not being 
                        completed quickly enough to cover the entire population 
                        due to issues with scheduling SIS assessments", 
                        em(
                          "(i.e. it's possible that you have enough people, with 
                          enough capacity, but there aren't appointments being 
                          scheduled in an efficient enough manner to keep 
                          everyone busy)"
                        ),
                        br(),
                        strong("Current Interviewers:"), 
                        "The default min and max for the staffing slider is 
                        based on the average number of assessments per 
                        interviewer over the past 3 months.",
                        br(),
                        strong("FTEs dedicated to SIS:"),
                        "The projection assumes that each interviewer has the 
                        same proportion of their position dedicated to SIS 
                        interviews, though this may not be accurate.  People may 
                        have other jobs besides SIS assessment, in which case it 
                        is not accurate to assume that everyone has the same 
                        capacity. Adding data about FTEs dedicated to SIS 
                        assessments would allow for this to be more accurate."
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "support",
        fluidRow(
          column(
            width = 6,
            box(
              title = "Support Needs Index", 
              status = "warning",
              collapsible = TRUE, 
              width = NULL,
              tabBox(
                width = NULL,
                tabPanel(
                  "Distribution",
                  radioButtons(
                    "central",
                    label = "Display:",
                    choices = c("Mean", "Median"), 
                    selected = "Mean",
                    inline = T
                  ),
                  plotlyOutput("hist_sni")
                ),
                tabPanel(
                  "Normal?",
                  plotlyOutput("norm_sni")
                ),
                tabPanel(
                  "Breakdown",
                  dataTableOutput("dt_sni")
                ),
                tabPanel(
                  "About", 
                  tabBox(
                    width = NULL,
                    tabPanel(
                      "SIS Support Needs Scale",
                      p(
                        "The Support Needs Scale (Section 1 of the SIS) consists 
                        of 49 activities grouped into six domains: ",
                        em(
                          "Home Living, Community Living, Lifelong Learning, 
                          Employment, Health and Safety, and Social Activities."
                        ), 
                        "The ", em("Support Needs Index"), "is a composite score 
                        that reflects a person’s overall intensity of support 
                        needs across these domains, relative to others with 
                        developmental disabilities."
                      ),
                      p(
                        "The SIS also measures exceptional Medical and Behavioral 
                        Support Needs, assessing 15 medical conditions and 12 
                        problem behaviors. Since certain medical conditions and 
                        challenging behaviors may require additional support, 
                        these items indicate cases where the ", 
                        em("Support Needs Index"), " may not fully reflect a 
                        person’s need for support."
                      )
                    ),
                    tabPanel(
                      "Distribution",
                      p(
                        "The graph shown in the 'Distribution' tab is called a 
                        histogram. A histogram groups numeric data into bins, 
                        displaying the bins as columns. They are used to show the 
                        distribution of a dataset, i.e. how often values fall 
                        into ranges."
                      ),
                      p(
                        "The histogram here shows the distribution of scores for 
                        all people assessed within the region.  Different colors 
                        are used to indicate the various agencies, and the dotted 
                        line shows the average for the selected agency"
                      )
                    ),
                    tabPanel(
                      "Normal?",
                      p(
                        "If you've been working with people who analyze data, you 
                        may have heard them use terms like", 
                        em("bell curve"), "or", em("normal distribution"), ".  ",
                        "That's what this chart looks at."
                      ),
                      p(
                        "The ", em("Support Needs Index"), " is normalized, which 
                        means that it is designed in such a way that its mean is 
                        100 and its standard deviation is 15, based on the 
                        initial group of people who were tested using the tool.  
                        If the ", 
                        em("Actual Scores"), " of the current population were 
                        distributed in the same way, they would fit under the ", 
                        em("bell curve"), "in the same way as the ", 
                        em("Standard Scores"), ".  Please note that agencies 
                        should not expect their data to match the bell curve 
                        until all of their population has been assessed."
                      ),
                      p(
                        "A density plot (which is what the lines are called) can 
                        be tricky to explain.  You can think of it as a smoothed 
                        out histogram.  The y-axis measurement ", em("Density"),
                        " is also different.  It boils down to this: the area 
                        under the entire curve is 1, and the probability of a 
                        value being between any two points on the x-axis is the 
                        area under the curve between those two points.  If you 
                        hover over a single point on the x-axis that says 80 and the 
                        y-axis says .01, what that means is that there is a probability 
                        of getting an x value of 80 around 1% of the time.",
                        "For a more technical explanation, check out the ",
                        a(href = "https://en.wikipedia.org/wiki/Probability_density_function",
                          "probability density function"
                        ), 
                        "."
                      )
                    ),
                    tabPanel(
                      "Breakdown",
                      p(
                        "The table in the 'Breakdown' tab shows the average scores 
                        for clients assessed by each agency across the sub-scales of 
                        the SIS which make up the ",
                        em("Support Needs Index"), ".  You can also look at the 
                        number of assessments completed by each agency."
                      ),
                      p(
                        "The width of the colored bars in the table corresponds to 
                        the numeric value within the cell, allowing an at-a-glance 
                        comparison of average scores in each sub-scale.  The table 
                        can be sorted by the values in any column."
                      ),
                      p(
                        "Clicking on the green 'plus' signs next to each row allows 
                        you to see the values for columns that don't fit in the 
                        view.  To change which columns are visible, click 'Show/Hide 
                        Columns' in the upper right corner."
                      )
                    )
                  )
                )
              )
            )
          ),
          column(
            width = 6,
            box(
              title = "Type of Service", 
              status = "warning",
              collapsible = TRUE, 
              width = NULL,
              tabBox(
                width = NULL,
                tabPanel(
                  "Table",
                  dataTableOutput("s1_dt")
                ),
                tabPanel(
                  "Chart",
                  uiOutput('s1domain'),
                  parsetOutput("tos_s1")
                ),
                tabPanel(
                  "About",
                  tabBox(
                    width = NULL,
                    tabPanel(
                      "The Chart", 
                      br(),
                      strong("This wobbly-looking chart..."),
                      p(
                        "Take a deep breath.  Don't run away screaming from the 
                        'dancing octopus' chart that you see here. Let's take a 
                        moment to understand how it might help us out."
                      ),
                      p(
                        "Have you ever asked a question sounding like: ",
                        em(
                          "What percent of people are ____? What percent of those 
                          people are _____?"
                        ), 
                        " and so on...?  If so, then this is the visualization for 
                        you."
                      ), 
                      p(
                        "With this chart, you can ask multiple questions like:",
                        br(),
                        em(
                          "In the area of money management, how many people needed 
                          prompting on a daily basis?  How many hours per day did 
                          they need it?"
                        ),
                        br(),
                        em(
                          "In the area of protection from exploitation, how many 
                          people needed full physical support on a monthly basis? 
                          How many minutes did they need it?"
                        )
                      ),
                      p(
                        "When you click on", em("alpha"), " or ", em("size"), 
                        ", the chart sorts the variable alphabetically or by size.  
                        You can sort either of these from low-to-high (<<) or 
                        high-to-low (>>)."
                      ),
                      p(
                        "The different colors on the chart correspond to the 
                        variable at the top of the chart, so if 'Type of Support' is 
                        on the top, there will be one color from each type of 
                        support weaving down through the other variables."
                      )
                    ),
                    tabPanel(
                      "The Data", 
                      br(),
                      strong("Type of Support"),
                      p(
                        "When completing Section 1, the support needs for each 
                        activity addressed in the section are examined 
                        with regard to three measures of support need:",
                        br(),
                        em("Frequency:"), 
                        "How often extraordinary support (i.e., support beyond that 
                        which is typically needed by most individuals without 
                        disabilities) is required for each targeted activity.",
                        br(),
                        em("Daily Support Time:"), 
                        "Amount of time that is typically devoted to support 
                        provision on those days when the support is provided.",
                        br(),
                        em("Type of Support:"), 
                        "The nature of support that would be needed by a person to 
                        engage in the activity in question."
                      ),
                      strong("Definitions"),
                      p(
                        "The chart uses short words and phrases for ease of use. 
                        Here are the full definitions from the SIS tool itself ",
                        a(
                          href = "http://aaidd.org/docs/default-source/sis-docs/sisfrequencyandscoringclarifications.pdf?sfvrsn=2",
                          "as defined by AAIDD"
                        ),":",
                        br(),
                        em("Frequency:"),
                        "Hourly = hourly or more frequently; 
                        Daily = at least once a day but not once an hour; 
                        Weekly = at least once a week, but not once a day; 
                        Monthly = at least once a month, but not once a week; 
                        None = none or less than monthly",
                        br(),
                        em("Daily Support Time (DST):"),
                        "Over 4 hrs = 4 hours or more; 
                        2-4 hrs = 2 hours to less than 4 hours; 
                        Under 2 hrs = 30 minutes to less than 2 hours; 
                        Under 30 min = less than 30 minutes; 
                        None = None"
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "protection",
        fluidRow(
          column(
            width = 6,
            box(
              title = "Type of Service", 
              status = "warning",
              collapsible = TRUE, 
              width = NULL,
              tabBox(
                width = NULL,
                tabPanel(
                  "Summary",
                  dataTableOutput("s2_dt")
                ),
                tabPanel(
                  "Chart",
                  uiOutput('s2domain'),
                  parsetOutput("tos_s2")
                ),
                tabPanel(
                  "About",
                  tabBox(
                    width = NULL,
                    tabPanel(
                      "The Chart", 
                      br(),
                      strong("This wobbly-looking chart..."),
                      p(
                        "Take a deep breath.  Don't run away screaming from the 
                        'dancing octopus' chart that you see here. Let's take a 
                        moment to understand how it might help us out."
                      ),
                      p(
                        "Have you ever asked a question sounding like: ",
                        em(
                          "What percent of people are ____? What percent of those 
                          people are _____?"
                        ), 
                        " and so on...?  If so, then this is the visualization for 
                        you."
                        ), 
                      p(
                        "With this chart, you can ask multiple questions like:",
                        br(),
                        em(
                          "In the area of money management, how many people needed 
                          prompting on a daily basis?  How many hours per day did 
                          they need it?"
                        ),
                        br(),
                        em(
                          "In the area of protection from exploitation, how many 
                          people needed full physical support on a monthly basis? 
                          How many minutes did they need it?"
                        )
                      ),
                      p(
                        "When you click on", em("alpha"), " or ", em("size"), 
                        ", the chart sorts the variable alphabetically or by size.  
                        You can sort either of these from low-to-high (<<) or 
                        high-to-low (>>)."
                      ),
                      p(
                        "The different colors on the chart correspond to the 
                        variable at the top of the chart, so if 'Type of Support' is 
                        on the top, there will be one color from each type of 
                        support weaving down through the other variables."
                      )
                    ),
                    tabPanel(
                      "The Data", 
                      br(),
                      strong("Type of Support"),
                      p(
                        "When completing Section 2, the support needs for each 
                        activity related to protection and advocacy are examined 
                        with regard to three measures of support need:",
                        br(),
                        em("Frequency:"), 
                        "How often extraordinary support (i.e., support beyond that 
                        which is typically needed by most individuals without 
                        disabilities) is required for each targeted activity.",
                        br(),
                        em("Daily Support Time:"), 
                        "Amount of time that is typically devoted to support 
                        provision on those days when the support is provided.",
                        br(),
                        em("Type of Support:"), 
                        "The nature of support that would be needed by a person to 
                        engage in the activity in question."
                      ),
                      strong("Definitions"),
                      p(
                        "The chart uses short words and phrases for ease of use. 
                        Here are the full definitions from the SIS tool itself ",
                        a(
                          href = "http://aaidd.org/docs/default-source/sis-docs/sisfrequencyandscoringclarifications.pdf?sfvrsn=2",
                          "as defined by AAIDD"
                        ),":",
                        br(),
                        em("Frequency:"),
                        "Hourly = hourly or more frequently; 
                        Daily = at least once a day but not once an hour; 
                        Weekly = at least once a week, but not once a day; 
                        Monthly = at least once a month, but not once a week; 
                        None = none or less than monthly",
                        br(),
                        em("Daily Support Time (DST):"),
                        "Over 4 hrs = 4 hours or more; 
                        2-4 hrs = 2 hours to less than 4 hours; 
                        Under 2 hrs = 30 minutes to less than 2 hours; 
                        Under 30 min = less than 30 minutes; 
                        None = None"
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "med-beh",
        fluidRow(
          box(
            title = "Filters", 
            status = "warning",
            collapsible = TRUE, 
            collapsed = TRUE, 
            tabBox(
              width = NULL,
              tabPanel(
                "Living situation(s):",
                selectInput(
                  "living",
                  label = NULL,
                  choices = levels(unique(scrub_sis$LivingType)), 
                  selected = levels(unique(scrub_sis$LivingType)), 
                  multiple = TRUE
                )
              ),
              tabPanel(
                "About",
                h4("Living situation(s)"),
                p(
                  "The options for this filter are groupings of the ",
                  em("Living Situation"), " field, combined as follows:",
                  br(),
                  strong("Facility:"), "Includes the following living situations:",
                  em(
                    "Adult Foster Care home certified, Agency-provided 
                    residential home with 4 to 6 people, Agency provided 
                    residential home with 10 or more people, General residential 
                    AFC NOT certified, Institutional setting, 
                    Prison/jail/juvenile detention center, Specialized 
                    residential AFC"
                  ),
                  br(),
                  strong("Family:"), "Includes the following living situations:",
                  em(
                    "Foster family home, Living with family, Private residence 
                    with family members"
                  ),
                  br(),
                  strong("Independent:"), "Includes the following living situations:",
                  em(
                    "Homeless, Living independently with supports, Private 
                    residence alone or with spouse or non-relatives, Private 
                    residence owned by the PIHP/CMHSP or provider"
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            box(
              title = "How Many Medical & Behavioral Needs?", 
              status = "warning",
              collapsible = TRUE, 
              width = NULL,
              tabBox(
                width = NULL,
                tabPanel(
                  "Distribution",
                  radioButtons(
                    "radio_mbhist",
                    label = "Display:",
                    choices = c("Medical", "Behavioral"), 
                    selected = "Medical",
                    inline = T
                  ),
                  plotlyOutput("hist_mb")
                ),
                tabPanel(
                  "About",
                  p(
                    "The graph shown in the 'Distribution' tab is called a 
                    histogram. A histogram groups numeric data into bins, 
                    displaying the bins as columns. They are used to show the 
                    distribution of a dataset, i.e. how often values fall into 
                    ranges.  Here, each bin has a range of one, so each column 
                    shows how many people have 1 condition, 2 conditions, and so 
                    on."
                  ),
                  p(
                    "The histogram here shows the distribution of conditions for 
                    people assessed within the region.  Since we're looking at 
                    medical and behavioral conditions which require a 
                    substantial amount of support, it isn't surprising that most 
                    people have only a few of these, falling on the left side of 
                    the histogram."
                  ),
                  p(
                    "When you select 'All' agencies, different colors are used 
                    to indicate the various agencies.  You can hover over the 
                    columns of histogram to see the exact number of people 
                    represented by each bin."
                  )
                )
              )
            )
          ),
          column(
            width = 6,
            box(
              title = "By Type", 
              status = "warning",
              collapsible = TRUE, 
              width = NULL,
              tabBox(
                width = NULL,
                tabPanel(
                  "Graph",
                  radioButtons(
                    "radio_mb",
                    label = "Display:",
                    choices = c("Medical", "Behavioral", "Both"), 
                    selected = "Both",
                    inline = T
                  ),
                  plotlyOutput("conditions")
                ),
                tabPanel(
                  "About",
                  h4("Section 3 (a.k.a. Exceptional Medical and Behavioral Support Needs)"),
                  p(
                    "Certain medical conditions and challenging behaviors are 
                    related to increased levels of support, regardless of 
                    support needs in other life areas. This section of the SIS 
                    looks at 15 medical conditions and 13 problem behaviors 
                    commonly associated with intellectual disabilities. "
                  ),
                  h4("Scoring"),
                  p(
                    "The question that the items in this section attempt to 
                    answer is:",
                    em(
                      "How significant are the following medical/behavioral 
                      conditions for the extra support required for this 
                      person?"
                    )
                  ),
                  p(
                    "Each item in the section is scored with one of the 
                    following ratings:",
                    br(),
                    strong("No support needed (0) "),
                    "No support needed because the condition is not an issue, or 
                    requires no support", 
                    br(),
                    strong("Some Support Needed (1) "),
                    "Some support needed to address the condition(s).  People 
                    who support must be continuously aware of the condition to 
                    assure the individual’s health and safety.",
                    br(),
                    strong("Extensive Support Needed (2) "),
                    "Extensive support is needed to address the condition, such as significant physical effort or time required."
                  ),
                  br(),
                  em("Based on ",
                     a(
                       href = "https://www.ascendami.com/ami/Portals/1/VA%20SIS%20Resources/Supports_Intensity_Scale.pdf",
                       "guidance published by AAIDD"
                    )
                  )
                )
              )
            )
          ) 
        )
      ),
      tabItem(
        tabName = "inter_rater",
        fluidRow(
          column(
            width = 6,
            box(
              title = "Subscale Comparison", 
              status = "warning",
              collapsible = TRUE, 
              width = NULL,
              tabBox(
                width = NULL,
                tabPanel(
                  "Boxplot",
                  uiOutput('box_opts'),
                  plotlyOutput("box_int")
                ),
                tabPanel(
                  "About",
                  h4("Boxplots..."),
                  p(
                    "The boxplots here show a summary of scores for all current 
                    interviewers.  A boxplot shows key information about the 
                    distribution of a measure, i.e. how it is spread out.  It is 
                    made up of the following pieces:",
                    br(),
                    strong("Median: "), 
                    "The mid-point of the data is shown by the line that divides 
                    the box into two parts. Half the scores are greater than or 
                    equal to this value, half are less.",
                    br(),
                    strong("Interquartile range: "), 
                    "The middle 'box' represents the middle 50% of scores for 
                    the group. The range of scores from lower to upper quartile 
                    is referred to as the inter-quartile range.",
                    br(),
                    strong("Upper quartile: "), 
                    "75% of the scores fall below the upper quartile. This is 
                    the top of the box (or the right side if the boxplot is 
                    displayed horizontally",
                    br(),
                    strong("Lower quartile: "), 
                    "25% of scores fall below the lower quartile. This is the 
                    bottom (left side) of the box.",
                    br(),
                    strong("Whiskers: "), 
                    "The whiskers stretch to the greatest (top) and least 
                    (bottom) values in the data, except for outliers.",
                    br(),
                    strong("Outliers: "), 
                    "Outliers are defined as more than 1.5x the upper value or 
                    less than 1.5x the lower value shown by the whiskers.",
                    br(),
                    "For more information, here's a ",
                    a(href = "http://flowingdata.com/2008/02/15/how-to-read-and-use-a-box-and-whisker-plot/",
                      "diagram from FlowingData"),
                    "showing the parts of a boxplot."
                  ),
                  br(),
                  h4("Interpreting them..."),
                  p(
                    "Box plots that are comparatively short show that an 
                    interviewer's scores fall within a restricted range. 
                    Comparatively tall box plots show a broader range of scores. 
                    If one box plot is much higher or lower than all the others, 
                    this may suggest either a difference between individuals 
                    being assessed or some variation in the way that the 
                    assessor is scoring individuals."
                  ),
                  p(
                    "Please recall that a broader range of scores by one 
                    interviewer could be due to characteristics of the group 
                    they assessed and is not automatically a concern with the 
                    validity of scoring."
                  )
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "planning",
        fluidRow(
          column(
            width = 6,
            box(
              title = "Items for Focus", 
              status = "warning",
              collapsible = TRUE, 
              collapsed = FALSE,
              width = NULL,
              tabBox(
                width = NULL,
                tabPanel(
                  "Support Needs",
                  uiOutput('import'),
                  selectInput(
                    "need_import_s1_measure",
                    label = "Show the:",
                    choices = c("Number of people with need", 
                                "Average level of need"), 
                    selected = "Number of people with need"
                  ),
                  plotlyOutput("need_import_s1")
                ),
                tabPanel(
                  "Important To/For",
                  plotlyOutput("import_s1")
                ),
                tabPanel(
                  "Protection",
                  plotlyOutput("plans")
                ),
                tabPanel(
                  "About",
                  tabBox(
                    width = NULL,
                    tabPanel(
                      "Support Needs",
                      p(
                        "This chart shows all life domains ", 
                        em("i.e. questions"), " where individuals either 
                        had a need or which they endorsed as important.  It 
                        excludes item responses where no need existed ", 
                        em("and"), " the item was not endorsed as important 
                        since these instances are unlikely to be relevant for 
                        planning."
                      )
                    ),
                    tabPanel(
                      "Important To/For",
                      p(
                        strong("Important to the Person"), 
                        "Each item in this section can be endorsed as being of 
                        particular importance either ", em("to"), " or ", 
                        em("for"), " the person served.  Items endorsed by the 
                        individual are particularly relevant for service 
                        planning, as they are likely to be high priorities for 
                        action."
                      ),
                      p(
                        strong("Important for the Person"),
                        "Items marked as being important ", em("for"), 
                        " the person have been identified by family members or 
                        other informants as relevant for the individual.  These 
                        items may be relevant  for planning, as they indicate 
                        areas where key members of an individuals support system 
                        have noted needs."
                      )
                    ),
                    tabPanel(
                      "Protection",
                      p(
                        "Section II of the SIS (the Supplemental Protection and 
                        Advocacy Scale) measures 8 activities such as: ",
                        em(
                          "self-advocacy, money management, protecting self from 
                          exploitation, legal responsibilities, making choices and 
                          advocating for others."
                        ), 
                        "The score from this section is not used to determine the ", 
                        em("Support Needs Index"), " score."
                        ),
                      p(
                        "The top 4 items in this section of the SIS are intended to 
                        be included in person-centered planning.  This graph shows 
                        which items most frequently make it to the", 
                        em("top 4"), "list of clients in the selected agency."
                      ),
                      p(
                        "CMHSPs may be interested in identifying how these areas 
                        are addressed through the development of individual 
                        plans of service (IPOS)."
                      )
                    )
                  )
                )
              )
            )
          ),
          column(
            width = 6,
            box(
              title = "Population Needs", 
              status = "warning",
              collapsible = TRUE, 
              collapsed = FALSE,
              width = NULL,
              tabBox(
                width = NULL,
                tabPanel(
                  "Patterns of Need",
                  tabBox(
                    width = NULL,
                    tabPanel(
                      "Defining Patterns",
                      p(
                        "To design programs that meet people where they are, it 
                        helps to understand patterns in the types of needs that 
                        people have in various areas of their lives. You can 
                        check out the ", em("Scenarios"), " tab for examples of 
                        situations where this may be useful."  
                      ),
                      h4("How many groups of people? (Rows)"),
                      p(
                        "Depending on your particular situation, you may want to 
                        focus on greater or fewer groups of clients, each of 
                        whom is depicted as a row in the heatmap.  You can 
                        select the number of groups here.  This will 
                        color the clusters of the groups of clients whose 
                        patterns of need are most distinct, based on the SIS 
                        subscales:"
                      ),
                      numericInput(
                        inputId = "need_rows",
                        label = NULL, 
                        value = 5,
                        min = 1, 
                        max = 10,
                        width = '100px'
                      ),
                      h4("How many clusters of needs? (Columns)"),
                      p(
                        "You may also want to see which types of need are more 
                        or less closely related in the population that has been 
                        assessed.  Selecting the number below will color the 
                        clusters of the need categories (columns):"
                      ),
                      numericInput(
                        inputId = "need_cols",
                        label = NULL, 
                        value = 5,
                        min = 1, 
                        max = 10,
                        width = '100px'
                      ),
                      p(
                        "Then, click on the ", em("Heatmap"), " tab to explore 
                        the groups in your population."
                      )
                    ),
                    tabPanel(
                      "Scenarios",
                      p(
                        "The following scenarios provide examples of instances 
                        where it may be helpful to define patterns of need in 
                        the population being served:"
                      ),
                      h4("Specialized Teams"), 
                      p(
                        "  A supervisor of a supports coordination team for 
                        persons with I/DD would like to start a pilot program
                        providing intensive, multi-disciplinary team based 
                        integrated care for quadrant four consumers (persons 
                        with high behavioral health and physical health needs 
                        as defined in the Four Quadrant Clinical Integration 
                        Model). She could use the heat map to identify a group 
                        of patients for whom this intervention could be offered.",
                        em("(In this instance, if she were trying to assign people 
                           to 3 supports coordination programs, she may want to 
                           highlight 3 groups)")
                      ),
                      h4("Best Practice Guidelines"), 
                      p(
                        "  A clinical director would like to identify clinical 
                        guidelines to assist clinicians in recommending best 
                        practices based on peoples needs. A first step could be 
                        taking the domains listed on the heat map and 
                        identifying any evidence based or best practice 
                        interventions that meet that particular need for people 
                        with I/DD."
                      ),
                      h4("Training Needs"), 
                      p(
                        "  A clinical director would like to know what types of 
                        trainings would benefit the clinicians serving persons 
                        with I/DD at his agency. He could look at the heat map 
                        and determine which three areas represent the highest 
                        need within the entire I/DD population served."
                      )
                    )
                  )
                ),
                tabPanel(
                  "Heatmap",
                  d3heatmapOutput("need_heat")
                ),
                tabPanel(
                  "About",
                  tabBox(
                    width = NULL,
                    tabPanel(
                      "Heatmap",
                      p(
                        "The visualization here is called a ",
                        a(
                          href = "https://en.wikipedia.org/wiki/Heat_map",
                          "heatmap"
                        ),
                        ".  This one shows the most recent scores for each 
                        client who has received a SIS assessment. The values 
                        of each subscale have been ", 
                        a(
                          href = "https://stat.ethz.ch/R-manual/R-devel/library/base/html/scale.html",
                          "normalized"
                        ),
                        " to allow for comparison.  Darker blue means a higher 
                        score, while lighter blue means a lower score. You can 
                        click and drag over cells to zoom in and look more 
                        closely at a given set."),
                      p(
                        "Here you can look at broader patterns of need across life 
                        domains for the current population of clients who have 
                        been assessed.  Clustering is an exploratory technique.  
                        It won't give you any conclusive results, but may 
                        generate insights and additional questions for analysis.  
                        The root-like shapes on the sides of the heatmap are 
                        called", 
                        a(
                          href = "http://wheatoncollege.edu/lexomics/files/2012/08/How-to-Read-a-Dendrogram-Web-Ready.pdf",
                          "dendrograms"
                        ),
                        "and they show how the different elements (here, clients 
                        and subscales) are grouped.  To allow you to more easily 
                        see these groupings, the heatmap allows you to select a 
                        number of groups (colors) to highlight for both the rows 
                        and columns."
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)