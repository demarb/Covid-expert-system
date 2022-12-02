:-dynamic symptom/2.
:-dynamic underlying_Cond/1.
:-dynamic user/5.
/*
Artifical Intelligence Project - Covid Expert System
Group Memebers & ID:
Demar Brown 1901599
Brianna Philp 1903475
Stephan Williams 1901898
Toniann Osbourne 1704288
*/

%Main Menu to be ran at command line
main_menu:-nl,write('COVID EXPERT SYSTEM MENU'),nl,
   write('Are you Ministry of Health or User?'),nl,
   write('Enter (1) for MOH OR (2) for User OR (3) to exit program'),nl,
   read(Choice),
   ((Choice==1) -> moh_subMenu;
   (Choice==2) -> user_subMenu;
   (Choice==3) -> write('END OF COVID EXPERT PROGRAM');
   (nl, write('INVALID ENTRY')), nl, nl, main_menu).

%Ministry of Health Sub Menu
moh_subMenu:-nl,nl,write('WECLOME TO COVID EXPERT SYSTEM MOH SUB MENU'),nl,
   write('OPTIONS [Select the corresponding number] : '), nl,
   write('(1) : Add Symptom to database'),nl,
   write('(2) : Add Underlying Conditions to database'),nl,
   write('(3) : Generate Report'),nl,
   write('(4) : Get Latest Advice for Current Data'),nl,
   write('(5) : Display all Symptoms'),nl,
   write('(6) : Display all Underlying Conditions'),nl,
   write('(7) : Return to Main Menu'),nl,
   read(Choice),
   ((Choice==1) -> accept_symptom;
   (Choice==2) -> accept_cond;
   (Choice==3) -> get_report;
   (Choice==4) -> get_advice;
   (Choice==5) -> displayAllSymptoms;
   (Choice==6) -> displayAllUnderlyingCond;
   (Choice==7) -> (write('RETURNING TO MAIN MENU'),nl, main_menu);
   (nl, write('INVALID ENTRY')), nl, nl, moh_subMenu).

%User Sub menu
user_subMenu:-nl,nl, write('WELCOME TO COVID EXPERT SYSTEM USER SUB MENU'),nl,
   write('OPTIONS [Select the corresponding number] : '), nl,
   write('(1) : Check If You Possibly Have Covid'),nl,
   write('(2) : Return to Main Menu'),nl,
   read(Choice),
   ((Choice==1) -> accept_user_detail;
   (Choice==2) -> (write('RETURNING TO MAIN MENU'),nl, main_menu);
   (nl, write('INVALID ENTRY')), nl, nl, user_subMenu).

%Built in symptoms
symptom(of_type(fever,mild),belongs_to(['Original'|[ 'Omicron', 'Gamma']])).
symptom(of_type(cough,mild),belongs_to(['Original'|[ 'Omicron', 'Gamma']])).
symptom(of_type(fatigue,mild),belongs_to(['Original'|[ 'Omicron', 'Gamma']])).
symptom(of_type('Sore Throat',mild),belongs_to(['Original'|[ 'Omicron', 'Gamma']])).
symptom(of_type(headache,mild),belongs_to(['Original'|[ 'Omicron', 'Gamma']])).
symptom(of_type('Muscle Aches',mild),belongs_to(['Original'|[ 'Omicron', 'Gamma']])).
symptom(of_type('Pressure in the chest',severe),belongs_to(['Original'|[ 'Omicron', 'Gamma']])).
symptom(of_type('Sudden confusion',severe),belongs_to(['Original'|[ 'Omicron', 'Gamma']])).
symptom(of_type('Inability to wake or stay awake',severe),belongs_to(['Original'|[ 'Omicron', 'Gamma']])).

symptom(of_type('Loss of taste or smell',mild),belongs_to(['Original'| 'Gamma'])).

symptom(of_type('Gastrointestinal tract issues',mild),belongs_to(['Gamma'])).
symptom(of_type('Coryza- Inflamed mucous membrane',mild),belongs_to(['Gamma'])).
symptom(of_type('Dyspnea- difficulty breathing',severe),belongs_to(['Gamma'])).

symptom(of_type('Runny nose',mild),belongs_to(['Omicron'])).
symptom(of_type('Nasal Congestion',mild),belongs_to(['Omicron'])).

symptom(of_type(diarrhea,mild),belongs_to(['Original'])).
symptom(of_type('Skin rash',mild),belongs_to(['Original'])).
symptom(of_type('Discolouration of fingers or toes',severe),belongs_to(['Original'])).
symptom(of_type('Red or irritated eyes',mild),belongs_to(['Original'])).

%Accepts a symptom from MOH
accept_symptom:-nl,
   write('Enter symptom : '), nl,
    read(Sym_in),nl,
    write('What variant does your symptom apply to?'),nl,
    write('(1)-Original Variant Only,(2)-Omicron Only,(3)-Gamma Only,(4)-Original + Omicron,(5)-Original + Gamma,(6)-Omicron + Gamma,(7)-All 3 Variants,'),nl,
   read(Category_in), nl,
   write('Is the symptom (1)Mild or (2)Severe?'),nl,
   read(Severity_in),write('INPUT ACCEPTED'),
   store_symptom(Sym_in, Category_in, Severity_in).

%Stores accepted symptom to database
store_symptom(Sym_in, Category_in, Severity_in):-
   ((Category_in==1) -> Category = ['Original'];
   (Category_in==2) -> Category = ['Omicron'];
   (Category_in==3) -> Category = ['Gamma'];
   (Category_in==4) -> Category = ['Original'|'Omicron'];
   (Category_in==5) -> Category = ['Original'| 'Gamma'];
   (Category_in==6) -> Category = ['Omicron'| 'Gamma'];
   (Category_in==7) -> Category = ['Original'|[ 'Omicron', 'Gamma']]),
   ((Severity_in==1) -> Severity = 'mild';
   (Severity_in==2) -> Severity = 'severe'),
   %Assertz adds symptom to knowledge base
    assertz(symptom(of_type(Sym_in,Severity),belongs_to(Category))),
    nl, write('SYMPTOM ADDED TO DATABASE'),nl,
     write('CURRENT LIST OF SYMPTOMS : '),nl,
   displayAllSymptoms.

%Built in Underlying Conditions
underlying_Cond('Asthma').
underlying_Cond('cerebrovascular disease').
underlying_Cond('Cancer').
underlying_Cond('chronic kidney disease').
underlying_Cond('chronic lung disease').
underlying_Cond('chronic liver disease').
underlying_Cond('cystic fibrosis').
underlying_Cond('Dementia').
underlying_Cond('Diabetes').
underlying_Cond('heart conditions').
underlying_Cond('HIV').
underlying_Cond('mental health disorder').
underlying_Cond('Obesity').
underlying_Cond('primary immunodeficiencies').
underlying_Cond('Transplantation').
underlying_Cond('tuberculosis').

%Accepts a new underlying condition from user
accept_cond:-nl,write('Enter underlying conditions : '), nl,
    read(Cond_in),nl,
    assertz(underlying_Cond(Cond_in)), displayAllUnderlyingCond.

get_report:-nl, write('GENERATING REPORT'),nl,nl,

   (   user(name(F_name), general_data(TempCel, SpecialSymp, IsLowPressure), hasSeverity(User_Mild, User_Severe), hasVariants(User_Original, User_Omicron, User_Gamma), countUnderlying(User_Condition)),
   nl,write('Name: '), write(F_name),  write(' -Temperature: '),write(TempCel),  write(' -Any special symptoms? '),write(SpecialSymp),  write(' -Has Low Pressure: '),write(IsLowPressure),  write(' -Has Mild Symptoms: '),write(User_Mild),  write(' -Has Severe Symptoms: '),write(User_Severe),  write(' -Has Original Variant: '),write(User_Original),  write(' -Has Original variant: '),write(User_Omicron),  write(' -Has gamma variant: '),write(User_Gamma),  write(' -# of Underlying Conditions: '),write(User_Condition),nl,nl,false);save_symp_toFile.


get_advice:-nl, write('GENERATING ADVICE'),nl.

%Displays all symptoms in the database
displayAllSymptoms:-
   (symptom(of_type(Sym_in,Severity),belongs_to(Category)),
   nl,write(Sym_in), write('-'), write(Severity), write('-'),  write(Category), false); moh_subMenu.

%Displays all underlying conditions in the database
displayAllUnderlyingCond:- (underlying_Cond(Condition),
   write(Condition), write(', '), false); moh_subMenu.

%Accepts user data to check for covid
accept_user_detail:-nl, write('DO YOU HAVE COVID CHECKER? - Please answer the following questions'),nl,
   write('Enter your first name'),
   read(F_name),nl,
   write('Enter your temperature in Fahrenheits'),
   read(TempFhr),
   TempCel is ((TempFhr - 32)* (5/9)),
   write(TempCel),
   nl, write('Are you experiencing any of the following symptoms: dizziness, fainting or blurred vision'),
   write('Enter (1) for Yes OR (2) for No'),
   read(Choice),
   ((Choice==1) -> SpecialSymp = 'Yes';
   (Choice==2) -> SpecialSymp = 'No'),
   nl, write(SpecialSymp),
   nl, write('Enter your blood pressure - systolic value'),nl,
   read(Sys_pressure),
   nl, write('Enter your blood pressure - diastolic  value'),nl,
   read(Dias_pressure),
   (((Sys_pressure<90),(Dias_pressure<60)) -> IsLowPressure = 'Yes';
   IsLowPressure = 'No'),
   nl, write(IsLowPressure),
   nl,checkSymptoms(User_Mild, User_Severe, User_Original, User_Omicron, User_Gamma),
   nl, checkUnderlyingCond(User_Condition),
   nl, checkForCovid(F_name, TempCel, SpecialSymp, IsLowPressure, User_Mild, User_Severe, User_Original, User_Omicron, User_Gamma, User_Condition).

%ALlows us to check if X exists within some list
in_list(X,[X|_]).
in_list(X,[_|T]) :- in_list(X,T).


%Checks and sets the categories of symptoms user may have
checkSymptoms(User_Mild, User_Severe, User_Original, User_Omicron, User_Gamma):-(
   (nl, nl, write('Are you experiencing any of the following mild symptoms: - select (1) for Yes and (2) for No'),nl,
   (symptom(of_type(Sym_in,Severity),belongs_to(Category)),
    (Severity == mild),
    write(Sym_in),nl, false);
   nl, read(MildChk),
    (MildChk == 1 -> User_Mild = 'Yes';
    MildChk == 2 -> User_Mild = 'No'),
    write('User has Mild symptoms?: ') ,write(User_Mild)),

   (nl, nl, write('Are you experiencing any of the following severe symptoms: - select (1) for Yes and (2) for No'),nl,
   (symptom(of_type(Sym_in,Severity),belongs_to(Category)),
    (Severity == severe),
    write(Sym_in),nl, false);
   nl, read(SevereChk),
    (SevereChk == 1 -> User_Severe = 'Yes';
    SevereChk == 2 -> User_Severe = 'No'),
    write('User has Severe symptoms?: ') ,write(User_Severe)),

    (nl, nl, write('Are you experiencing any of the following original variant symptoms: - select (1) for Yes and (2) for No'),nl,
   (symptom(of_type(Sym_in,Severity),belongs_to(Category)),
    in_list('Original', Category),    write(Sym_in),nl, false);
    nl, read(OriginalVariantChk),
    (OriginalVariantChk == 1 -> User_Original = 'Yes';
    OriginalVariantChk == 2 -> User_Original = 'No'),
    write('User has Original Variant symptoms?: ') ,write(User_Original)),

    (nl, nl, write('Are you experiencing any of the following omicron variant symptoms: - select (1) for Yes and (2) for No'),nl,
   (symptom(of_type(Sym_in,Severity),belongs_to(Category)),
    in_list('Omicron', Category),    write(Sym_in),nl, false);
    nl, read(OmicronVariantChk),
    (OmicronVariantChk == 1 -> User_Omicron = 'Yes';
    OmicronVariantChk == 2 -> User_Omicron = 'No'),
    write('User has Omicron Variant symptoms?: ') ,write(User_Omicron)),

    (nl, nl, write('Are you experiencing any of the following gamma variant symptoms: - select (1) for Yes and (2) for No'),nl,
   (symptom(of_type(Sym_in,Severity),belongs_to(Category)),
    in_list('Gamma', Category),    write(Sym_in),nl, false);
    nl, read(GammaVariantChk),
    (GammaVariantChk == 1 -> User_Gamma = 'Yes';
    GammaVariantChk == 2 -> User_Gamma = 'No'),
    write('User has Gamma Variant symptoms?: ') ,write(User_Gamma))

).

%Checks underllying conditions the user may have
checkUnderlyingCond(User_Condition):-
   (nl, nl, write('Have you ever been diagnosed with any of the following conditions: - How many have you been diagnosed with?'),nl,
   (underlying_Cond(Condition),
    write(Condition),nl, false);
   nl, read(User_Condition),
   write('User has underlying Condition? COUNT: ') ,write(User_Condition)).

%Test user
user(name('ben'), general_data(33, 'Yes', 'Yes'), hasSeverity('Yes', 'No'), hasVariants('No', 'Yes', 'No'), countUnderlying(2)).

% Checks which covid user may have and provides advice based on their type and symptom severity
checkForCovid(F_name, TempCel, SpecialSymp, IsLowPressure, User_Mild, User_Severe, User_Original, User_Omicron, User_Gamma, User_Condition):-
   nl,write('RESULTS: '),nl,
   (nl,write('You possible have the following Variants: '),nl,
   ((User_Original == 'Yes'),(User_Omicron == 'Yes'), (User_Gamma == 'Yes')) -> (write('Any of the three Variant'),nl);
   ((User_Original == 'Yes'),(User_Omicron == 'Yes')) -> (write('Original or Omicron Variant'),nl);
   ((User_Original == 'Yes'),(User_Gamma == 'Yes')) -> (write('Original or Gamma Variant'),nl);
   ((User_Omicron == 'Yes'), (User_Gamma == 'Yes')) -> (write('Omicron or Gamma Variant'),nl);
   ((User_Original == 'Yes')) -> (write('Original Variant'),nl);
   ((User_Omicron == 'Yes')) -> (write('Omicron Variant'),nl);
   ((User_Gamma == 'Yes')) -> (write('Gamma Variant'),nl);
   ((User_Original == 'Yes'),(User_Omicron == 'Yes'), (User_Gamma == 'Yes')) -> (write('No Variant Detected. You likely do not have Covid!.'),nl,nl)),

   ( (User_Mild=='Yes')-> (nl,write('ADVICE: Mild symptoms were detected. Self isolate and get tested as soon as possible'));
   write('---')),
   ( (User_Severe=='Yes')-> (nl,write('ADVICE: Severe symptoms were detected. Call ahead to your local emergency facility. Urgent care may be needed.')) ;
   write('---')),

   (((User_Omicron == 'Yes'), (User_Condition>3))-> (nl,write('IMPORTANT: More than 3 underlying conditions were detected. You face significant risk of hospitalization and Death because the Omicron Variant was detected')) ;
   write('---')),


   (assertz(user(name(F_name), general_data(TempCel, SpecialSymp, IsLowPressure), hasSeverity(User_Mild, User_Severe), hasVariants(User_Original, User_Omicron, User_Gamma), countUnderlying(User_Condition)))),

   user_subMenu.



save_symp_toFile:-
   open('users.txt', write, Stream),
   findall((F_name, TempCel, SpecialSymp, IsLowPressure, User_Mild, User_Severe, User_Original, User_Omicron, User_Gamma, User_Condition), user(name(F_name), general_data(TempCel, SpecialSymp, IsLowPressure), hasSeverity(User_Mild, User_Severe), hasVariants(User_Original, User_Omicron, User_Gamma), countUnderlying(User_Condition)),AllUsers),
   write(Stream, AllUsers), nl,
  close(Stream).



/*
gen_allSymptoms(Sym_in,Severity, Category,AllSymp):-
   findall((Sym_in,Severity, Category), gen_allSymptoms(Sym_in,Severity, Category,AllSymp):-
   findall((Sym_in,Severity, Category), symptom(of_type(Sym_in,Severity),belongs_to(Category)),AllSymp),
   write(AllSymp), AllSymp),
   write(AllSymp).

save_symp_toFile:-
   open('users.txt', write, Stream),
   findall((Sym_in,Severity, Category), symptom(of_type(Sym_in,Severity),belongs_to(Category)),AllSymp),
   write(Stream, AllSymp), nl,
  close(Stream).

read_Symp:-
   open('symptoms.txt', read, Stream),
   get_char(Stream, Char1),
   process_stream(Char1, Stream),
   close(Stream).

process_stream(end_of_file, _):- !.

process_stream(Char, Stream):-
   write(Char),
   get_char(Stream, Char2),
   process_stream(Char2, Stream).


save_symp_toFile2:-
   open('symptoms-test.txt', write, Stream),
   forall(symptom(of_type(Sym_in,Severity),belongs_to(Category)), write(Stream, symptom(of_type(Sym_in,Severity),belongs_to(Category)))), nl,
  close(Stream).

*/
