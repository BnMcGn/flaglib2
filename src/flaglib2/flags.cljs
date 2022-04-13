(ns flaglib2.flags
  ;;(:require)
  )

;;Flag info in a format that should be readable from multiple lisps


;;; Negative:
(def flag-src
  '((:id :negative-spam
         :label "Spam"
         :category "Negative"
         :color "#f00"
         :description "For the obvious use on any comments that contain out of place marketing. It can also be used on URLs that are referred to by spam emails. Don't use it for offtopic posts. There is a flag for that.")

    (:id :negative-inflammatory
         :label "Inflammatory"
         :category "Negative"
         :color "#ff8100"
         :description "For content that is inflammatory or offensive in tone, especially if you feel that its point could have been made more gently. Name-calling should be labelled with this flag.")

    (:id :negative-disagree
         :label "Disagree"
         :category "Negative"
         :color "#ff8100"
         :description "A generic way to say \"I think you are wrong\".")

    (:id :negative-dislike
         :label "Dislike"
         :category "Negative"
         :color "#ff8100"
         :description "The most generic downvote. This one should not be taken too seriously. Some people will use it for decluttering and other sorting tasks.")

    (:id :negative-language-warning
         :label "LanguageWarning"
         :category "Negative"
         :color "#ffe843"
         :description "For offensive or off-color language.")

    (:id :negative-disturbing
         :label "Disturbing"
         :category "Negative"
         :color "#f00"
         :description "Warning that a link is pornographic, disturbing, or otherwise not safe for work.")

    (:id :negative-already-answered
         :label "AlreadyAnswered"
         :category "Negative"
         :color "#ffe843"
         :description "If you feel that a point has been answered well in another place, use this flag's reference field to point to the existing answer, rather than restating it
in place. Argument can continue at the site of the existing answer. Using this
flag will help prevent redundant discussions all over the place.")

    (:id :negative-logical-fallacy
         :label "LogicalFallacy"
         :category "Negative"
         :color "#ff8100"
         :description "Use this to point out a logical fallacy. It's good form to explain the
problem in your comment.")

    (:id :negative-needs-evidence
         :label "NeedsEvidence"
         :category "Negative"
         :color "#4f41c8"
         :description "Any time that a vague claim has been made, such as \"Studies indicate...\".
This is your way of saying \"Put up or shut up\" to an opponent. It is a way
to challenge unquestioned assumptions.")

    (:id :negative-evidence
         :label "Evidence"
         :category "Negative"
         :color "#00adff"
         :description "You are presenting evidence that rebuts an item or provides an answer to a NeedsEvidence flag.")

    (:id :negative-raise-question
         :label "RaiseQuestion"
         :category "Negative"
         :color "#4f41c8"
         :description "When you feel that a point has not been covered. For example, when a reporter
has failed to explore the background and associations of a source. When an
angle that would derail an argument has been avoided.")

    (:id :negative-out-of-bounds
         :label "OutOfBounds"
         :category "Negative"
         :color "#f00"
         :description "Meant to restrain some of the worst of online forum behavior. Specifically, when you see someone post immediately damaging information, this is a request for its prompt removal. For example, when someone has posted another person's home address, or is using the forum to directly organize a physical attack. This flag is for serious situations only. If you don't understand it, don't use it.")

;;; Positive:

    (:id :positive-funny
         :label "Funny"
         :category "Positive"
         :color "#1cff00"
         :description "For the evident use. Note: Some people don't feel that the upvoting of Funny
 comments adds to a discussion, therefore they may regard a positive Funny vote as a proxy for Offtopic.")

    (:id :positive-agree
         :label "Agree"
         :category "Positive"
         :color "#1cff00"
         :description "Generic agreement: When you think that something is right.")

    (:id :positive-like
         :label "Like"
         :category "Positive"
         :color "#1cff00"
         :description "Even more generic approval: Use this for things that are a matter of taste more than a matter of fact or opinion.")

    (:id :positive-interesting
         :label "Interesting"
         :category "Positive"
         :color "#00adff"
         :description "Use this when you wish to indicate that something is worth noting, even if you aren't ready to agree with it.")

    (:id :positive-evidence
         :label "Evidence"
         :category "Positive"
         :color "#00adff"
         :description "You are presenting evidence that supports an item.")

;;; Custodial:

    (:id :custodial-redundant
         :label "Redundant"
         :category "Custodial"
         :color "#ffe843"
         :description "This flag is for decluttering. It won't have a negative effect on other people's scores. It only means that it's content has been better stated elsewhere. It's polite to put a link to your prefered statement in the Reference
field.")

    (:id :custodial-out-of-date
         :label "OutOfDate"
         :category "Custodial"
         :color "#ffe843"
         :description "Some comments are going to need pruning as time progresses. When appropriate, add a link to updated statements using the Reference field.")

    (:id :custodial-retraction
         :label "Retraction"
         :category "Custodial"
         :color "#9a12c6"
         :description "This flag is only valid for use against your own posts. Use it to acknowledge
that you were wrong or out of line about something. It can also be used for cleaning up accidental posts.")

    (:id :custodial-correction
         :label "Correction"
         :category "Custodial"
         :color "#ffe843"
         :description "Use this on spelling and grammatical errors. WARNING: some reader systems may use this flag to automatically update other people's posts. It is therefore possible to use this flag to tamper with the meaning of them. This is not
the purpose of this flag. It is meant for innocuous changes. Subversive use may
result in the censure of the community!")

    (:id :custodial-incorrect-flag
         :label "IncorrectFlag"
         :category "Custodial"
         :color "#ffe843"
         :description "Use this when someone has misused a flag, and has evidently done
so out of ignorance. This warns reader systems to ignore comments that might
throw the discussion tree out of whack.")

    (:id :custodial-flag-abuse
         :label "FlagAbuse"
         :category "Custodial"
         :color "#f00"
         :description "A very serious claim. We can expect that in partisan wrath, people will try to discredit others by misusing certain flags, eg. by labelling an honest opinion as Spam or using the Correction flag to change another person's words. This flag is a yell out that someone is fighting dirty. It's a call for the community to investigate.")

    (:id :custodial-offtopic
         :label "Offtopic"
         :category "Custodial"
         :color "#ffe843"
         :description "When you feel that a comment doesn't belong in a discussion. Different
 people have different levels of tolerance for offtopic discussion, and will set
 their readers to allow or ignore it.")

    (:id :custodial-arcane
         :label "Arcane"
         :category "Custodial"
         :color "#ffe843"
         :description "Use this flag when you check out of a conversation because it is
 deteriorating into name calling or getting trapped in details.")

    (:id :custodial-blank
         :label "Blank"
         :category "Custodial"
         :color "#fff"
         :description "The most general purpose flag. Can be used to mark an excerpt for later reference.")))

(def flags (into {}
                 (for [flag flag-src]
                   [(second flag)
                    (into {}
                          (partition 2 flag))])))

(def flaglist (map second flag-src))
