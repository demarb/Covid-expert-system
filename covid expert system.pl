:-dynamic symptom/2.
:-dynamic underlying_Cond/1.
/*
Artifical Intelligence Project - Covid Expert System
Group Memebers & ID:

*/

main_menu:-nl,write('COVID EXPERT SYSTEM MENU'),nl,
   write('Are you Ministry of Health or User?'),nl,
   write('Enter (1) for MOH OR (2) for User OR (3) to exit program'),nl,
   read(Choice),
   ((Choice==1) -> moh_subMenu;
   (Choice==2) -> user_subMenu;
   (Choice==3) -> write('END OF COVID EXPERT PROGRAM');
   (nl, write('INVALID ENTRY')), nl, nl, main_menu).


moh_subMenu:-nl,write('WECLOME TO COVID EXPERT SYSTEM MOH SUB MENU'),nl,
   write('OPTIONS [Select the corresponding number] : '), nl,
   write('(1) : Add Symptom to database'),nl,
   write('(2) : Add Underlying Conditions to database'),nl,
   write('(3) : Generate Report'),nl,
   write('(4) : Get Latest Advice for Current Data'),nl,
   write('(5) : Return to Main Menu'),nl,
   read(Choice),
   ((Choice==1) -> accept_symptom;
   (Choice==2) -> accept_cond;
   (Choice==3) -> get_report;
   (Choice==4) -> get_advice;
   (Choice==5) -> (write('RETURNING TO MAIN MENU'),nl, main_menu);
   (nl, write('INVALID ENTRY')), nl, nl, moh_subMenu).


user_subMenu:-nl, write('WELCOME TO COVID EXPERT SYSTEM USER SUB MENU'),nl,
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

accept_symptom:-nl,write('Enter symptom : '), nl,
    read(Sym_in),nl,
    write('What variant does your symptom apply to?'),nl,
    write('(1)-Original Variant Only,(2)-Omicron Only,(3)-Gamma Only,(4)-Original + Omicron,(5)-Original + Gamma,(6)-Omicron + Gamma,(7)-All 3 Variants,'),nl,
   read(Category_in), nl,
   write('Is the symptom (1)Mild or (2)Severe?'),nl,
   read(Severity_in),write('INPUT ACCEPTED'),
   store_symptom(Sym_in, Category_in, Severity_in).


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
    assertz(symptom(of_type(Sym_in,Severity),belongs_to(Category))).



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

accept_cond:-nl,write('Enter underlying conditions : '), nl,
    read(Cond_in),nl,
    assertz(underlying_Cond(Cond_in)).


get_report:-nl, write('GENERATING REPORT'),nl.

get_advice:-nl, write('GENERATING ADVICE'),nl.


accept_user_detail:-nl, write('DO YOU HAVE COVID CHECKER? - Please answer the following questions'),nl,
   write('Enter your temperature in Fahrenheits').













%CreateList:-
   %append(['All Symptoms'], [symptom(of_type(Sym_in,Severity),belongs_to(Category))], allSymptoms).


gen_allSymptoms(Sym_in,Severity, Category,AllSymp):-
   findall((Sym_in,Severity, Category), symptom(of_type(Sym_in,Severity),belongs_to(Category)),AllSymp),
   write(AllSymp).


save_symp_toFile:-
   open('symptoms.txt', write, Stream),
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

