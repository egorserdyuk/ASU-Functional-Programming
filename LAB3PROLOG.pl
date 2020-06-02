department(1, sentry(101, "Davis Yorkie Mackenzie", "psychiatrist", "1-551-276-8880"), date(7, 4, 2020),
           [sick("Grande Ann Petrovic", 23, "flu", "st. Alamo 9 3/4"), sick("Cyrus Miley Ray", 27, "covid-19", "st. Kennedy 43"), sick("Cyrus Billy Ray", 58, "sars-cov-2", "st. Kennedy 44")],
           [brigade("14475-EV", doctor("Brown Victor Sven", "therapist"), "Grownfund Ron Joshua"), brigade("00910-TF", doctor("Gray Liam Lone", "therapist"), "Wreckler Noah Taff")]).

department(2, sentry(189, "Davis Kelly Mbatha-Raw", "phlebotomist", "1-254-366-1490"), date(8, 4, 2020),
           [sick("Firebanks Elv Closen", 14, "flu", "st. Castro 664"), sick("Cyrus Noah Ray", 21, "flu", "st. Kennedy 42"), sick("Svenson George Terry", 34, "covid-19", "st. Fahrenheit 451")],
           [brigade("42424-LT", doctor("McGlower Sean Helen", "therapist"), "Grownfund Ron Joshua"), brigade("44112-IO", doctor("Fry Phillip Jay", "therapist"), "Perimeter Cube Willie")]).

department(3, sentry(32, "Harrison Ford Joseph", "pediatrician", "1-307-111-0932"), date(9, 4, 2020),
           [sick("Williams Grace Trophy", 66, "flu", "st. Hacker Way 1"), sick("Taylor Daisy Ron", 20, "pneumonia", "st. Jan Rokotv 10"), sick("Price Natalie Perry", 31, "flu", "st. Ocean Drive 12")],
           [brigade("14475-EV", doctor("Brown Victor Sven", "therapist"), "Tamp Hugo Taylor"), brigade("32846-FX", doctor("Ridley Scott Freddie", "therapist"), "Jankovsky Roman Niko")]).

department(4, sentry(42, "McFly George Martin", "surgeon", "1-012-534-7869"), date(10, 4, 2020),
           [sick("Shown Kenny Ivanovic", 24, "mers", "st. Preach 312"), sick("Harvey Li Osvald", 26, "flu", "st. Query 5")],
           [brigade("42424-LT", doctor("Palmer Lora Yves", "therapist"), "Xan Patrick Stewart"), brigade("32280-SL", doctor("Breadshow Yuri Paul", "therapist"), "Qwerty Ratiex Titan")]).

count([], _, 0).
count([sick(_, _,H, _) | T], Desease, Count):- count(T, Desease, Count1),
    ((H=Desease, Count is Count1+1); (not(H=Desease), Count is Count1)).

summary([], _, 0, 0).
summary([sick(_, Age, H, _) | Tail], Desease, Count, Result) :-
    summary(Tail, Desease, Count1, Result1),
    ((H=Desease, Result is Result1+Age, Count is Count1+1); (not(H=Desease), Result is Result1, Count is Count1)).

avg_age(ListSick, Desease, Avg) :-
    summary(ListSick, Desease, Count, Summ),
    Avg is round(Summ / Count).

print_list([]).
print_list([Head | Tail]) :-
   write(Head), nl,
   print_list(Tail).

f(Desease, Result) :-
   findall(
       [Day, Month, Year, ShiftDay, SentryName, SentryPosition, CountSick, AverageAge],

      (
         department(ShiftDay, sentry(_, SentryName, SentryPosition, _), date(Day, Month, Year), ListSick, _),
         count(ListSick, Desease, CountSick),
         avg_age(ListSick, Desease, AverageAge)
      ),

      Result
   ),

	print_list(Result), nl.


/** <examples>
?- f("flu", R).
*/
