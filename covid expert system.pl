/*
Artifical Intelligence Project - Covid Expert System
Group Memebers & ID:

*/


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
   ((Severity_in==1) -> Severity = 'Mild';
   (Severity_in==2) -> Severity = 'Severe'),
   %Assertz adds symptom to knowledge base
    assertz(symptom(of_type(Sym_in,Severity),belongs_to(Category))).

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


