Final report
================
Soren Rosier
2019-06-04

  - [Background](#background)
  - [The study](#the-study)
  - [Findings](#findings)
      - [Trained 7th Graders Far Outperform Other
        Groups](#trained-7th-graders-far-outperform-other-groups)
      - [Trained 7th grade helpers have more content knowledge and
        higher performing
        tutees](#trained-7th-grade-helpers-have-more-content-knowledge-and-higher-performing-tutees)
      - [The PeerTeach training improves helping, no matter the helper’s
        content mastery or the learner’s math
        ability](#the-peerteach-training-improves-helping-no-matter-the-helpers-content-mastery-or-the-learners-math-ability)
      - [What we can learn from incorporating interactional data from
        the PeerTeach training
        website](#what-we-can-learn-from-incorporating-interactional-data-from-the-peerteach-training-website)

## Background

In 1984, Charles Bloom and his doctoral students ran an experiment: they
had one group of students learn via lecture and another via mastery
learning and tutoring. After scoring an assessment at the end of the
experiment, Bloom found, “The average tutored student was above 98% of
the students in the control class” (p. 5), which is a difference of two
full standard deviations. Bloom states, “This is the 2 sigma problem.
Can researchers and teachers devise teaching-learning conditions that
will enable the majority of students under group instruction to attain
levels of achievement that can at present be reached only under good
tutoring conditions?” My research asks, Why not give every student good
tutoring conditions? Why not convert every classmate into a tutor for
the content they have mastered? Why not give every learner the
opportunity for one-on-one instruction via a peer?

The simplest reason “why not” is that children tend to make ineffectual
tutors for their peers. The small number of studies aiming to
characterize peer tutors have consistent findings: tutors tend to do
much more explaining than tutees (King, 1997), place minimal demand on
tutees when questioning (Graesser et al., 1995), and rarely stimulate
deep-level reasoning or do much to monitor the understanding of tutees
(Graesser et al. 1995; Roscoe and Chi 2007).

## The study

PeerTeach is a web application I developed to train kids to be effective
teachers. The hope is that, after just 30 minutes interacting with the
training, students will be able to more effectively teach their peers.
\~170 6th and 7th graders participated in the most recent experiment,
all believing they were being trained to be effective helpers. On Day 1
of the study, Group 1 received a WISE psychological intervention aimed
at getting students to commit to asking more questions; Group 2 received
a game-based training on asking good questions; Group 3 (the control
group) was told that knowing more math helps you become a better teacher
then they practiced doing math problems.

On Day 2 of the study, each class was split in half to learn different
content: either comparing means and medians or comparing rates. On Day
3, students taught the content they learned the prior day to a student
of the same training group who learned the other lesson. After 20
minutes of peer teaching, a post assessment was administered.

Did the training work? Did trained students in Groups 1 and 2 “teach”
more effectively?

## Findings

### Trained 7th Graders Far Outperform Other Groups

For the 7th graders, the treatment interventions appear to have made a
huge difference in their teaching ability. Among 7th graders, the median
post-assessment score of students taught by “trained” helpers was
actually double the median score of students taught by control students.
However, the scores of 7th grade control students are suspiciously low.
Could some other intervening variable explain the drastic difference in
scores between trained and untrained 7th graders?

``` r
#Give conditions better names, specify grade level
ptdata <- 
  ptdata %>% 
  mutate(
    group = 
      recode(
        group,
        "group1" = "Treatment 1",
        "group2" = "Treatment 2",
        "group3" = "Control"
      )
  ) %>% 
  mutate(
    grade_level = 
      if_else(
        period %in% c("leeson4", "leeson5", "leeson6"), 
        "7th Grade", 
        "6th Grade"
      )
  ) %>% 
  drop_na(total)
```

``` r
ptdata %>%
  ggplot(mapping = aes(x = group, y = total)) +
  geom_hline(aes(yintercept = median(total)), color = "blue") +
  geom_boxplot() +
  facet_wrap(~ grade_level) +
  labs(
    title = "Trained 7th grade helpers far outperform other groups",
    y = "Post assessment score of helped student",
    x = "Condition"
  )
```

![](step_6_final_report_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### Trained 7th grade helpers have more content knowledge and higher performing tutees

In order to understand whether the trained 7th graders were a
substantively different group than the untrained 7th graders, I sought
out standardized math test scores for each student. After combining that
information with data on post-assessment performance, it appears there
is a legitimate difference between conditions groups: the control group
7th graders have much lower standardized test scores.

``` r
ptdata %>% 
  left_join(stscores, by = c("username_partner" = "username")) %>% 
  filter(grade_level == "7th Grade") %>%
  drop_na(NWEA) %>% 
  ggplot(mapping = aes(y = NWEA, x = group)) +
  geom_boxplot() +
  labs(
    x = "Condition",
    y = "Standardized math test scores"
  )
```

![](step_6_final_report_files/figure-gfm/q2.1-1.png)<!-- -->

The challenge now is to determine whether differences in standardized
math test scores even correlate with post-assessment scores in this
study. Interestingly, it appears that helpers with higher standardized
test scores are not necessarily more effective at helping. Students with
higher test scores are, however, better at learning. Could this be the
intervening variable that explains the success of the trained 7th grade
helpers?

``` r
ptdata %>% 
  left_join(stscores, by = c("username_partner" = "username")) %>% 
  drop_na(NWEA) %>% 
  ggplot(mapping = aes(x = NWEA, y = total)) +
  geom_point(position = "jitter") +
  geom_smooth() +
  labs(
    x = "Standardized math test scores of helped student",
    y = "Post assessment score of helped student"
  )
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](step_6_final_report_files/figure-gfm/q2.2-1.png)<!-- -->

``` r
ptdata %>% 
  left_join(stscores, by = "username") %>% 
  drop_na(NWEA) %>% 
  #drop extreme test score outlier
  filter(NWEA != min(NWEA)) %>% 
  #filter(grade_level == "7th Grade") %>% 
  ggplot(mapping = aes(x = NWEA, y = total)) +
  geom_point(position = "jitter") +
  geom_smooth() +
  #facet_wrap(~ grade_level, scales = "free") +
  labs(
    x = "Standardized math test scores of helper",
    y = "Post assessment score of the helper"
  )
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](step_6_final_report_files/figure-gfm/q2.2-2.png)<!-- -->

### The PeerTeach training improves helping, no matter the helper’s content mastery or the learner’s math ability

We have established that the trained 7th grade helpers were teaching
students with higher standardized test scores and that there is a
relationship between standardized test scores and success on this
study’s post assessment. Do those higher standardized test scores
account for all of the differences in this study’s post-assessment
performance acrosss groups?

To test this possibility, we can compare post-assessment performance
between students with similar standardized-test scores. The data
suggests that, in almost every test score band, trained students
outperform control students as teachers. In the lowest band, though,
when helped students have standardized test scores equal to or below
200, it does not appear to matter whether the helper was trained. In all
conditions, trained and untrained, those low performing students were
not successfully taught by their peers.

These findings are both promising and worrisome: while this data
suggests that the PeerTeach training makes students more effective
helpers, irrespective of the ability of the students they are helping,
it also suggests that the most struggling math students are not very
teachable via peer.

``` r
ptdata %>% 
  left_join(stscores, by = c("username_partner" = "username")) %>%
  drop_na(NWEA) %>% 
  #filter(period %in% c("leeson4", "leeson5", "leeson6")) %>%
  filter(NWEA > 175) %>% 
  mutate(
    stscores_bracket =
      case_when(
        NWEA > 175 & NWEA <= 200 ~ "175 - 200",
        NWEA > 200 & NWEA <= 210 ~ "201-210",
        NWEA > 210 & NWEA <= 220 ~ "211-220",
        NWEA > 220 ~ "Over 220"
    )
  ) %>% 
  ggplot(mapping = aes(x = group, y = total)) +
  geom_hline(aes(yintercept = median(total)), color = "blue") +
  geom_boxplot(varwidth = TRUE) +
  facet_grid(cols = vars(stscores_bracket)) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0)) +
  labs(
    title = "Trained students teach more effectively no matter learner's math ability",
    subtitle = "Scores broken down by NWEA test score range",
    y = "Post assessment score of helped student",
    x = "Condition"
  )
```

![](step_6_final_report_files/figure-gfm/q2.3-1.png)<!-- -->

There is one more critical piece of data that might tell us how the
experimental conditions were different: after students were taught the
math content by adult teachers, and before they taught their peers, they
took a short quiz to indicate if they’d learned the material. Perhaps
peer teachers in the 7th grade control group never learned the content.
The data suggests this is likely not the case. Students in the control
group had similar pre-teaching quiz scores to students in Treatment 2
and lower scores than students in Treatment 1.

``` r
ptdata %>% 
  left_join(exittickets, by = "username") %>%
  drop_na(et_total) %>% 
  filter(grade_level == "7th Grade") %>% 
  ggplot(mapping = aes(x = group, y = et_total)) +
  geom_boxplot()
```

![](step_6_final_report_files/figure-gfm/q3-1.png)<!-- -->

Given that the 7th grade helpers in the control group and Treatment
Group 2 have very similar pre-teaching quiz scores, it seems unlikely
that the post-assessment performance gap was driven entirely by the
content knowledge of helpers. That said, it is still worth comparing
post-test performance of students with similar pre-teaching quiz scores
for two reasons: 1) there appears to be a difference between
pre-teaching quiz scores for the Treatment 1 group and the control
group, so we should validate that Treatment 1 is effective, regardless
of helper quiz scores, and 2) there is value in understanding the
relationship between helpers’ content knowledge and the learning of
their tutees.

After separating all helpers into pre-teaching quiz score bands, we find
that: 1) trained students are more effective helpers within every
content knowledge band, and 2) students with strong mastery of the math
content before teaching (those who scored avove 75%) who received the
PeerTeach training were much more effective helpers than every other
group of helpers. This suggests that peer teaching should only happen
between students who have both strong content understanding and
training. Both pieces appear critical.

``` r
ptdata %>% 
  left_join(exittickets, by = "username") %>%
  drop_na(et_total) %>% 
  mutate(
    et_bracket =
      case_when(
        et_total <= .25 ~ "0-25%",
        et_total > .25 & et_total <= .5 ~ "26-50%",
        et_total > .5 & et_total <= .75 ~ "51-75%",
        et_total > .75 ~ "76-100%"
    )
  ) %>% 
  ggplot(mapping = aes(x = group, y = total)) +
  geom_hline(aes(yintercept = median(total)), color = "blue") +
  geom_boxplot(varwidth = TRUE) +
  facet_grid(cols = vars(et_bracket)) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0)) +
  labs(
    title = "Trained students teach more effectively no matter their prior knowledge",
    subtitle = "The combination of high content mastery plus training makes vastly better helpers",
    y = "Post assessment score of helped student",
    x = "Condition"
  )
```

![](step_6_final_report_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

### What we can learn from incorporating interactional data from the PeerTeach training website

Perhaps the most critical determinant of teaching success is not which
condition students were in, but how well they developed the teaching
mindset and skillset that PeerTeach focuses on: asking good questions to
promote active engagement by the student one is helping.

All students in this study, those in both treatment conditions along
with the control students, finish their intervention experience by
playing an online game where they make teaching decisions. In this
level, they control a virtual peer tutor helping a virtual cartoon
learner. For each of 4 scenarios, students are presented with 3 options
for what to say. One move is an expert teaching move that elicits the
other student’s thinking. The two other options are more didactic
behaviors that shut down opportunities for the other student to think.
Many of these didactic speech options are cloaked in questions so that
students can’t “game” the system by just picking questions.

Students’ decisions in the game (their clicks) are captured by the
website and stored on a server. By extracting codes in the backend data
that represent the decisions students made, I was able to create a score
for each student that represents how often (out of 4 opportunities) they
selected the expert teaching move (i.e., the more elicitive move).

``` r
# This chunk scrapes data from peerteach.us, producing a tibble with just 2 variables: username and teaching score.
q4.1 <- 
  webdata %>%  
  mutate(
    username = str_remove(username, "[:digit:]"),
    comments2 = str_remove(comments, "\"learnometer\"")
  ) %>% 
  separate(
    comments2, 
    into = c("score", "decisions"), 
    sep = "\"path\":", 
    remove = TRUE
  ) %>% 
  mutate(
    score = str_remove_all(score, ","),
    score = str_remove_all(score, "\\{:"), #now you have a clean score!
    decisions = str_remove_all(decisions, "\""),
    decisions = str_remove(decisions, "\\["),
    decisions = str_remove(decisions, "\\]\\}")
  ) %>% 
  separate(
    decisions, 
    into = c("s1", "decide1", "decide2", "s2", "decide3", "decide4", "s3"),
    sep = ","
  ) %>% 
  mutate_at(
    #starts_with("deci"), 
    vars(decide1, decide2, decide3, decide4),
    ~ if_else(. %in% expert_teacher_moves, 1L, 0L)
  ) %>% 
  mutate(total_correct = decide1 + decide2 + decide3 + decide4) %>% 
  select(username, total_correct) %>% 
  group_by(username) %>% 
  summarize(total_correct = mean(total_correct) %/% 1)

q4.1
```

    ## # A tibble: 132 x 2
    ##    username   total_correct
    ##    <chr>              <dbl>
    ##  1 accurate               3
    ##  2 advice                 4
    ##  3 appreciate             4
    ##  4 Argentina              2
    ##  5 argued                 4
    ##  6 binoculars             1
    ##  7 butter                 3
    ##  8 cardigan               3
    ##  9 category               3
    ## 10 civilized              1
    ## # … with 122 more rows

The relatinship between online decision making scores and students’
in-person teaching success is quite interesting. While there does seem
to be correlation between students choosing expert elicitive teaching
moves online and serving as effective helpers in person, there are
diminishing returns for students who only choose elicitive helping
moves. It appears that the most effective helpers mostly use expert
elicitive teaching moves, but also sprinkle in enough didactic moves to
further support their peers. Students who chose elicitive moves 75% of
the time online had the most successful tutees in real life. Perhaps
that ratio represents the ideal questioning/explaining balance for peer
helpers.

``` r
ptdata %>% 
  left_join(q4.1, by = "username") %>% 
  drop_na(total_correct) %>% 
  ggplot(mapping = aes(x = as.factor(total_correct), y = total)) +
  geom_boxplot() +
  labs(
    title = "Students who choose expert elicitive moves online teach better",
    subtitle = "There are diminishing returns on questions; the best peer teachers balance some explanation",
    x = "Frequency students chose expert elicitive moves in tutoring simulation",
    y = "Post test score of student helped"
  )
```

![](step_6_final_report_files/figure-gfm/q4.2-1.png)<!-- -->
