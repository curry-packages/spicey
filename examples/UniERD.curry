import Database.ERD

uniERD :: ERD
uniERD =
 ERD "Uni"
 [Entity "Student" [Attribute "MatNum"    (IntDom Nothing) PKey False,
                    Attribute "Name"      (StringDom Nothing) NoKey False,
                    Attribute "Firstname" (StringDom Nothing) NoKey False,
                    Attribute "Email"     (StringDom Nothing) NoKey True],
  Entity "Lecture" [Attribute "Id"    (IntDom Nothing) PKey False,
                    Attribute "Title" (StringDom Nothing) Unique False,
                    Attribute "Hours" (IntDom (Just 4)) NoKey False],
  Entity "Lecturer" [Attribute "Id"        (IntDom Nothing) PKey False,
                     Attribute "Name"      (StringDom Nothing) NoKey False,
                     Attribute "Firstname" (StringDom Nothing) NoKey False],
  Entity "Group" [Attribute "Time" (StringDom Nothing) NoKey False]]
 [Relationship "Teaching"
               [REnd "Lecturer" "taught_by" (Exactly 1),
                REnd "Lecture"  "teaches"   (Between 0 Infinite)],
  Relationship "Participation"
               [REnd "Student" "participated_by" (Between 0 Infinite),
                REnd "Lecture" "participates"    (Between 0 Infinite)],
  Relationship "Membership"
               [REnd "Student" "consists_of" (Exactly 3),
                REnd "Group" "member_of"     (Between 0 Infinite)]]
