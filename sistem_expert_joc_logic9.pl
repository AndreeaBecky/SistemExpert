close_all:- current_stream(_,_,S),close(S),fail;true.
curata_bc:- current_predicate(P), abolish(P,[force(true)]), fail;true.

:- discontiguous citeste_cuvant/3, rest_cuvinte_propozitie/2, trad/3, executa/1, scopuri_princ/0, proceseaza_raspuns/3, interogheaza/4, realizare_scop/3, transformare2/2, transformare/2, cum/1.
:- use_module(library(lists)).
:- use_module(library(file_systems)).
:- use_module(library(system)).
:- use_module(library(file_systems),[]),file_systems:current_directory(_,'C:/Users/Becky/Desktop/Proiect Prolog').

:- op(900,fy,not).

:- dynamic fapt/3.
:- dynamic interogat/1.
:- dynamic scop/1.
:- dynamic interogabil/3.
:- dynamic regula/3.
:- dynamic descriere/4.


not(P) :-  P, !, fail.
not(_).

% SCRIE IN CONSOLA O LISTA (propozitie)
% -------------------------------------
scrie_lista([]):- nl.

scrie_lista([H|T]) :-  
	write(H), space(1), 
	scrie_lista(T).
	
	space(0).
		
	space(N) :-  
		N > 0,write(' '),
		N1 is N-1, space(N1).
		

% CITESTE UN CUVANT (de la tastatura / din fisier)
% ------------------------------------------------
citeste_cuvant(-1,end_of_file,-1):- !.

citeste_cuvant(Caracter,Cuvant,Caracter1) :-    
	caracter_cuvant(Caracter),!, 
	name(Cuvant, [Caracter]),get_code(Caracter1).
	
	caracter_cuvant(C):- member(C,[35,44,59,58,63,61,62,33,46,41,40,91,93, 123, 125, 126]).
    % am specificat ASCII pentru   #  ,  ;  :  ?  =  >  !  .  ) (  [	]	 {    }   ~


citeste_cuvant(Caracter, Numar, Caracter1) :- 
	caracter_numar(Caracter),!,
	citeste_tot_numarul(Caracter, Numar, Caracter1).
	
	caracter_numar(C):- C<58,C>=48.
		
	citeste_tot_numarul(Caracter,Numar,Caracter1):- 
		determina_lista(Lista1,Caracter1),
		append([Caracter],Lista1,Lista),
		transforma_lista_numar(Lista,Numar).

		determina_lista(Lista,Caracter1):- 
			get_code(Caracter), 
			(caracter_numar(Caracter),
			determina_lista(Lista1,Caracter1),
			append([Caracter],Lista1,Lista); 
			\+(caracter_numar(Caracter)),
			Lista=[],Caracter1=Caracter).
		 
		transforma_lista_numar([],0).
			
		transforma_lista_numar([H|T],N):- 
			transforma_lista_numar(T,NN), 
			lungime(T,L), Aux is exp(10,L),
			HH is H-48,N is HH*Aux+NN.

			lungime([],0).
				
			lungime([_|T],L):- 
				lungime(T,L1),
				L is L1+1.


citeste_cuvant(Caracter,Cuvant,Caracter1) :- 
	Caracter==39,!,  % 39 este codul ASCII pt '
	pana_la_urmatorul_apostrof(Lista_caractere),
	L=[Caracter|Lista_caractere],
	name(Cuvant, L),get_code(Caracter1).

	pana_la_urmatorul_apostrof(Lista_caractere):- 
		get_code(Caracter),
		(Caracter == 39,Lista_caractere=[Caracter];
		Caracter\==39,
		pana_la_urmatorul_apostrof(Lista_caractere1),
		Lista_caractere=[Caracter|Lista_caractere1]).


citeste_cuvant(Caracter,Cuvant,Caracter1) :-           
	caractere_in_interiorul_unui_cuvant(Caracter),!,              
	((Caracter>64,Caracter<91),!,
	Caracter_modificat is Caracter+32;
	Caracter_modificat is Caracter),                              
	citeste_intreg_cuvantul(Caractere,Caracter1),
	name(Cuvant,[Caracter_modificat|Caractere]).
    
	caractere_in_interiorul_unui_cuvant(C):- 
		C >= 48,C =< 57;  % [0,9]
		C >= 65,C =< 90;  % [A,Z]
		C >= 97,C =< 122; % [a,z]
		C == 45;		  % [-]
		C == 95;		  % [_]
		C == 47.		  % / Slash

	citeste_intreg_cuvantul(Lista_Caractere,Caracter1) :- 
		get_code(Caracter),
		(caractere_in_interiorul_unui_cuvant(Caracter),
		((Caracter>64,Caracter<91),!, 
		Caracter_modificat is Caracter+32;
		Caracter_modificat is Caracter),
		citeste_intreg_cuvantul(Lista_Caractere1, Caracter1),
		Lista_Caractere=[Caracter_modificat|Lista_Caractere1]; \+(caractere_in_interiorul_unui_cuvant(Caracter)),
		Lista_Caractere=[], Caracter1=Caracter).


citeste_cuvant(_,Cuvant,Caracter1) :-                 
	get_code(Caracter),       
	citeste_cuvant(Caracter,Cuvant,Caracter1).

%___________________________________________________________________________________________________________________________
%________________________S  T  A  R  T______________________________________________________________________________________
%___________________________________________________________________________________________________________________________

% PORNIRE program

pornire :- 
	retractall(interogat(_)),
	retractall(fapt(_,_,_)),
	repeat,
	write('Introduceti una dintre urmatoarele optiuni: '),
	nl,nl,
	write(' Incarca | Consulta | Afisare_tabelara | Afisare_fapte | Cum | Iesire '),
	nl,nl,write(':-  '),citeste_linie([H|T]),
	executa([H|T]), H == iesire.
	
	
	citeste_linie([Cuv|Lista_cuv]) :- 
		get_code(Car),
		citeste_cuvant(Car, Cuv, Car1), 
		rest_cuvinte_linie(Car1, Lista_cuv).
 
		rest_cuvinte_linie(-1, []):- !. % -1 este codul ASCII pt EOF
			
		rest_cuvinte_linie(Car,[]) :- (Car==13; Car==10; Car==35), !.
			
		rest_cuvinte_linie(Car,[Cuv1|Lista_cuv]) :- 
			citeste_cuvant(Car,Cuv1,Car1),      
			rest_cuvinte_linie(Car1,Lista_cuv).


% PORNIRE meniu SECUNDAR

pornire_secundar :-
	nl,
	write('Doresti sa afisezi si descrierile solutiilor?'),
	nl,nl,
	write(' (da nu) '),
	nl,nl,write('|- '), citeste_linie([H|T]),
	executa_secundar([H|T]),nl.

	executa_secundar([da]) :-
		afis_lista_detalii,nl,! .

	executa_secundar([nu]):-!.


	% INCARCA fisiere

	executa([incarca]) :-  
		incarca,!,nl,
		write('Fisierul dorit a fost incarcat!'), nl, nl.
	
		incarca :- 
			nl,
			% write('Introduceti numele fisierului cu reguli: '),nl, nl, write(':- '), %read(F),
			F1 = 'reguli.txt', file_exists(F1), !, incarca(F1, reguli),
			F2 = 'descrieri.txt', file_exists(F2), !, incarca(F2, descrieri).
			 
		incarca:- write('Nume incorect de fisier! '), nl, fail.
			
		incarca_fisier :- 
			repeat, citeste_propozitie(L),
			proceseaza(L),L == [end_of_file],nl.

		incarca(F, reguli) :- 
			retractall(interogat(_)),retractall(fapt(_,_,_)),
			retractall(scop(_)),retractall(interogabil(_,_,_)),
			retractall(regula(_,_,_)),
			see(F), incarca_fisier, seen, !.
			
		incarca(F, descrieri) :- 
			retractall(descriere(_,_,_,_)),
			see(F), incarca_fisier, seen, !.


				citeste_propozitie([Cuv|Lista_cuv]) :- 
					get_code(Car),citeste_cuvant(Car, Cuv, Car1), 
					rest_cuvinte_propozitie(Car1, Lista_cuv).
				 
					rest_cuvinte_propozitie(-1, []) :- !. % E O F 
						
					rest_cuvinte_propozitie(Car,[]) :- Car==46, !. % Punct
					
					rest_cuvinte_propozitie(Car,[Cuv1|Lista_cuv]) :- 
						citeste_cuvant(Car,Cuv1,Car1),      
						rest_cuvinte_propozitie(Car1,Lista_cuv).
					
					
				proceseaza([end_of_file]):- !.
					
				proceseaza(L) :-  trad(R,L,[]),assertz(R), !.
				

					trad(scop(X)) --> [scop,'=','(',X,')'].
					
						
					trad(interogabil(Atr,M,P)) --> 
						[intrebare, '=', '(',Atr,';'],afiseaza(Atr, P),lista_optiuni(M), [')'].
						
						lista_optiuni(M) --> [optiuni,'('],lista_de_optiuni(M).

							lista_de_optiuni([Element]) -->  [Element,')'].
								
							lista_de_optiuni([Element|T]) --> [Element,'#','#'],lista_de_optiuni(T).

						afiseaza(_,P) -->  [P].
							
						afiseaza(P,P) -->  [].
						

					trad(regula(N,premise(Daca),concluzie(Atunci,F))) --> identificator(N),daca(Daca),atunci(Atunci,F).
						
						identificator(N) --> [r,'=',N].
							
						daca(Daca) --> [cond,'['],lista_premise(Daca).

							lista_premise([Daca]) --> ['#'], propoz(Daca),[']','=','>'].
								
							lista_premise([Prima|Celalalte]) --> ['#'], propoz(Prima), lista_premise(Celalalte).

						atunci(Atunci,FC) --> propoz(Atunci), [avand,fc,'('],[FC],[')'].
							
						atunci(Atunci,100) --> propoz(Atunci).

							propoz(not av(Atr,da)) --> ['!',Atr].
								
							propoz(av(Atr,Val)) --> [Atr,'>','>',Val].
								
							propoz(av(Atr,da)) --> [Atr].
						
						
					%% Trad pentru parsare DESCRIERI
					
					trad(descriere(N, C, D, Daca)) --> identificator_descriere(N), [C, '#','#','#'], ['descriere', '=', '{', D, '}'], daca_descriere(Daca), ['/////////////////////////////////////////'].
						
						identificator_descriere(N) --> ['#','#','#', N, '#','#','#'].
						daca_descriere(Daca) --> ['proprietati', ':'], lista_premise_descriere(Daca).
						
							lista_premise_descriere([Daca]) --> ['#','#', '#', '{'], propoz_premisa_descriere(Daca), ['}'].
							lista_premise_descriere([Prima|Celalalte]) --> ['#','#', '#', '{'], propoz_premisa_descriere(Prima), ['}'], lista_premise_descriere(Celalalte).
						
								propoz_premisa_descriere(av(Atr,Val)) --> [Atr, '~', Val].
								propoz_premisa_descriere(not av(Atr,da)) --> [Atr, '~', 'nu'].
								propoz_premisa_descriere(av(Atr,da)) --> [Atr, '~', 'da'].								

					trad('Eroare la parsare'-L,L,_).

	
	% CONSULTA sistemul expert

	executa([consulta]) :-  
		retractall(interogat(_)),
		retractall(fapt(_,_,_)),
		scopuri_princ,!.

		% Afisare descriere format fisierul de intrare : N-scop, C-cale_relativa_imagine, D-descriere, L-lista_proprietati
			
		afiseaza_informatii(N, _):- descriere(N, C, D, L), nl,
		write(N), write(' ### '), write(C), write(' ###'), nl,
		write('descriere =  {'), write(D), write('}'), nl,
		write('propietati:'), nl, lista_premise_descriere(L), 
		write('/////////////////////////////////////////').

		afiseaza_informatii.
		
		lista_premise_descriere([]).
		lista_premise_descriere([L|T]):- propoz_premisa_descriere(L), lista_premise_descriere(T).

		propoz_premisa_descriere(av(Atr,Val)):- write('### {'), write(Atr), write(' ~ '), write(Val), write('}'), nl .
		propoz_premisa_descriere(not av(Atr,da)):- write('### {'), write(Atr), write(' ~ '), write('nu'), write('}'), nl .
		propoz_premisa_descriere(av(Atr,da)):- write('### {'), write(Atr), write(' ~ '), write('da'), write('}'), nl .	

		% ------------------------------------------------

			
		% Afiseaza in ordine descrescatoare dupa factorul de certitudine	
		
		scopuri_princ:-
			scop(Atr), (setof(st(FC,Atr,Val), Istorie^(determina(Atr), fapt(av(Atr,Val), FC, Istorie), FC>19),L) ->
																					afis_lista_sol(L),
																					pornire_secundar, !, nl,
																					afis_dem_lista_solutii(L), nl
																				; 
																					write('Ne pare rau, nu am gasit nicio solutie'), nl
						).
						
				afis_lista_sol([]):-!.
				
				afis_lista_sol([st(FC,Atr,Val)|T]) :- 
					afis_lista_sol(T),
					scrie_scop(av(Atr,Val),FC).
					
				afis_lista_sol_detalii([st(FC,Atr,Val)|T]) :- 
					afis_lista_sol(T),
					scrie_scop(av(Atr,Val),FC),
					afiseaza_informatii(Val,FC), nl.
					
				afis_lista_detalii :-
					scop(Atr),
					setof(st(FC,Atr,Val), Istorie^(determina(Atr), fapt(av(Atr,Val), FC, Istorie), FC>19),L),
					afis_lista_sol_detalii(L).
					
				% creeaza  un fisier intr-un folder cu demonstratia	
				afis_dem_lista_solutii([st(_, Atr, Val)|T]):-
					scop(Scop),
					(directory_exists(Scop) -> true;make_directory(Scop)),
					atom_concat(Scop, '/demonstratie@[', C0),
					atom_concat(C0,Val, C1),
					atom_concat(C1,']', C2),
					atom_concat(C2,'.txt', C3),
					tell(C3), cum(av(Atr, Val)), told,
					afis_dem_lista_solutii(T).
				afis_dem_lista_solutii([]):-!.


		scopuri_princ.
		
			determina(Atr) :- 
				realizare_scop(av(Atr,_),_,[scop(Atr)]),!.

			determina(_).
				
				realizare_scop(not Scop,Not_FC,Istorie) :- 
					realizare_scop(Scop,FC,Istorie),
					Not_FC is - FC, !.


				realizare_scop(Scop,FC,_) :- 
					fapt(Scop,FC,_), !.


				realizare_scop(Scop,FC,Istorie) :- 
					pot_interoga(Scop,Istorie),
					!,realizare_scop(Scop,FC,Istorie).
					
					pot_interoga(av(Atr,_),Istorie) :- 
						not interogat(av(Atr,_)),
						interogabil(Atr,Optiuni,Mesaj),
						nl,
						interogheaza(Atr,Mesaj,Optiuni,Istorie),
						asserta( interogat(av(Atr,_)) ).
						
						
						interogheaza(Atr,Mesaj,[da,nu],Istorie) :- 
							!,write(Mesaj),nl,
							citeste_opt(X, [da, nu, nu_stiu, nu_conteaza], Istorie),
							% se trateaza cazurile nu_stiu & nu_conteaza:
							(X == [nu_stiu] ->
												true
											;
												(X == [nu_conteaza]
													->
														asserta(fapt(av(Atr,da),100,[utiliz])),
														asserta(fapt(not av(Atr,da),100,[utiliz]))
													;
														det_val_fc(X, Atr, Fapt),
														asserta(Fapt)
												)
							).
												
							
							de_la_utiliz(X,Istorie,Lista_opt) :- 
								repeat, nl, write(':- '), citeste_linie(X),
								proceseaza_raspuns(X,Istorie,Lista_opt).

								proceseaza_raspuns([de_ce],Istorie,_) :-  afis_istorie(Istorie),!,fail.
									afis_istorie([]) :-  nl.
										
									afis_istorie([scop(X)|T]) :- 
										scrie_lista([scop,X]),!, afis_istorie(T).

									afis_istorie([N|T]) :- 
										afis_regula(N),!,afis_istorie(T).
									
								proceseaza_raspuns([X],_,Lista_opt):- 
									member(X,Lista_opt).
									
								proceseaza_raspuns([X,fc,FC],_,Lista_opt):- 
									member(X,Lista_opt),float(FC).

							
							% Factorul de certitudine nu se va mai salva: nu -> (da, -FC)
							
							det_val_fc([nu],Atr,Fapt) :- Fapt = fapt(not av(Atr), 100, [utiliz]), !.
							det_val_fc([nu,FC],Atr,Fapt) :- Fapt = fapt(not av(Atr), FC, [utiliz]), !.
							det_val_fc([nu,fc,FC],Atr,Fapt) :- Fapt = fapt(not av(Atr), FC, [utiliz]), !.

							det_val_fc([Val,FC],Atr,Fapt) :- Fapt = fapt(av(Atr, Val), FC, [utiliz]), !.					
							det_val_fc([Val,fc,FC],Atr,Fapt) :- Fapt = fapt(av(Atr, Val), FC, [utiliz]), !.
							det_val_fc([Val],Atr,Fapt) :- Fapt = fapt(av(Atr, Val), 100, [utiliz]), !.


						interogheaza(Atr,Mesaj,Optiuni,Istorie) :- 
							write(Mesaj),nl,
							append(Optiuni, [nu_stiu, nu_conteaza], ListaOptiuni),
							citeste_opt(VLista, ListaOptiuni, Istorie), 
							(VLista == [nu_stiu] ->
												true
											;
												(VLista == [nu_conteaza] 
													->
														adauga_toate_opt(Atr, Optiuni)
														
													;
														assert_fapt(Atr,VLista)
												)
							).
							
							citeste_opt(X,Optiuni,Istorie) :- 
								append(['('],Optiuni, Opt1),
								append(Opt1,[')'],Opt),
								scrie_lista(Opt),
								de_la_utiliz(X,Istorie,Optiuni).
								
							assert_fapt(Atr,[Val,fc,FC]) :- 
								!, asserta( fapt(av(Atr,Val),FC,[utiliz]) ).

							assert_fapt(Atr,[Val]) :- 
								asserta(fapt(av(Atr,Val), 100, [utiliz])).
								
							adauga_toate_opt(Atr, [H|T]) :- assert_fapt(Atr, [H]), adauga_toate_opt(Atr, T).
							adauga_toate_opt(_, []).


				realizare_scop(Scop,FC_curent,Istorie) :- 
					fg(Scop,FC_curent,Istorie).
					
					fg(Scop,FC_curent,Istorie) :- 
						regula(N, premise(Lista), concluzie(Scop,FC)),
						demonstreaza(N,Lista,FC_premise,Istorie),
						ajusteaza(FC,FC_premise,FC_nou),
						actualizeaza(Scop,FC_nou,FC_curent,N),
						FC_curent == 100,!.

					fg(Scop,FC,_) :-  fapt(Scop,FC,_).
						
						demonstreaza(N,ListaPremise,Val_finala,Istorie) :- 
							dem(ListaPremise,100,Val_finala,[N|Istorie]),!.

							dem([],Val_finala,Val_finala,_).
								
							dem([H|T],Val_actuala,Val_finala,Istorie) :- 
								realizare_scop(H,FC,Istorie),
								Val_interm is min(Val_actuala,FC),
								Val_interm >= 20,
								dem(T,Val_interm,Val_finala,Istorie).
				
						ajusteaza(FC1,FC2,FC) :- 
							X is FC1 * FC2 / 100,
							FC is round(X).
				
						actualizeaza(Scop,FC_nou,FC,RegulaN) :- 
							fapt(Scop,FC_vechi,_),
							combina(FC_nou,FC_vechi,FC),
							retract( fapt(Scop,FC_vechi,Reguli_vechi) ),
							asserta( fapt(Scop,FC,[RegulaN | Reguli_vechi]) ),!.

						actualizeaza(Scop,FC,FC,RegulaN) :- 
							asserta( fapt(Scop,FC,[RegulaN]) ).

							combina(FC1,FC2,FC) :- 
								FC1 >= 0,FC2 >= 0,
								X is FC2*(100 - FC1)/100 + FC1,
								FC is round(X).

							combina(FC1,FC2,FC) :- 
								FC1 < 0,FC2 < 0,
								X is - ( -FC1 -FC2 * (100 + FC1)/100),
								FC is round(X).

							combina(FC1,FC2,FC) :- 
								(FC1 < 0; FC2 < 0),
								(FC1 > 0; FC2 > 0),
								FCM1 is abs(FC1),FCM2 is abs(FC2),
								MFC is min(FCM1,FCM2),
								X is 100 * (FC1 + FC2) / (100 - MFC),
								FC is round(X).

			afiseaza_scop(Atr) :-
				nl,fapt(av(Atr,Val),FC,_),
				FC >= 20,scrie_scop(av(Atr,Val),FC),
				nl, fail.

			afiseaza_scop(_):- nl.

				scrie_scop(av(Atr,Val),FC) :- nl,
					transformare(av(Atr,Val), X),
					scrie_lista(X),space(2),
					write(' '),
					write('factorul de certitudine este '), 
					FC1 is integer(FC),write(FC1), nl.
			
					transformare(av(A,da),[A]) :-  !.
						
					transformare(not av(A,da), [not,A]) :-  !.
					transformare2(not av(A,da), ['!',A]) :-  !.
						
					transformare(av(A,nu),[not,A]) :-  !.
					transformare2(av(A,nu),['!',A]) :-  !.
						
					transformare(av(A,V),[A,este,V]).
					transformare2(av(A,V),[A,'>>',V]).
					
					
	matrice(matr,
				[
					['element 1', 'element 2', 'element 3'],
					['element 1', 'element 2', 'element 3'],
					['element 1', 'element 2', 'element 3']
				]
			).	
		
	executa([afisare_tabelara]) :-  
		afisare_tabelara, !.
		
		afisare_tabelara :- matrice(matr, _M), afisare_tabel(_M), nl, nl.
		
	
	afis_caracter(_, 0).
	afis_caracter(Chr, Nr):- Nr>0, write(Chr), Nr1 is Nr-1, afis_caracter(Chr, Nr1).
	afis_cap_tabel :- nl, format('|~t~a~t~15|~t~a~t~15+~t~a~t~15+~t|~50|~n',['Joc Logic', 'Factor Certitudine', 'Calculator?']).
	
	afisare_tabel(M):- afis_caracter('_', 50), nl, afis_cap_tabel, afis_caracter('_', 50), nl, afisare_mijloc(M), afis_caracter('_', 50), nl.
	
		afisare_mijloc([Linie|Rest]):- 
			nl, format('|~t~s~t~15|~t~s~t~15+~t~s~t~19+~t|~50|~n', Linie),
			nl, afisare_mijloc(Rest).
		afisare_mijloc([]).
		
	/*	
	executa([afisare_solutii]) :-  
		afisare_solutii, !.
		
		afisare_solutii :- 
		
			write('Fapte existente in baza de cunostinte:'),
			nl, afiseaza_scop(_). % Atr
	*/

	executa([afisare_fapte]) :- 
		afiseaza_fapte,!.

		afiseaza_fapte :- 
			write('Fapte existente in baza de cunostinte:'),
			nl,nl, write(' (Atribut, valoare) '), nl,nl,
			listeaza_fapte,nl.

			listeaza_fapte:-   
				fapt(av(Atr,Val),FC,_), 
				write('('),write(Atr),write(','),
				write(Val), write(')'),
				write(','), write(' certitudine '),
				FC1 is integer(FC),write(FC1),
				nl,fail.

			listeaza_fapte.


	executa([cum|L]) :-  cum(L),!.
		
		cum([]) :-  write('Mentionati si scopul '),nl,
			write(':- '),citeste_linie(Linie),nl,
			transformare(Scop,Linie), cum(Scop).

		cum(L) :-  
			transformare(Scop,L),nl, cum(Scop).

		cum(not Scop) :-  
			fapt(Scop,FC,Reguli),
			lista_float_int(Reguli,Reguli1),
			FC < -20,
			transformare(not Scop,PG),
			append(PG,[a,fost,derivat,cu,ajutorul,'regulilor: '|Reguli1],LL),
			scrie_lista(LL),nl,
			afis_reguli(Reguli),fail.

		cum(Scop) :- 
			fapt(Scop,FC,Reguli),
			lista_float_int(Reguli,Reguli1),
			FC > 20,
			transformare(Scop,PG),
			append(PG,[a,fost,derivat,cu,ajutorul,'regulilor: '|Reguli1],LL),
			scrie_lista(LL),nl,
			afis_reguli(Reguli),fail.

			lista_float_int([],[]).
				
			lista_float_int([Regula|Reguli],[Regula1|Reguli1]):- 
				(Regula \== utiliz,
				Regula1 is integer(Regula);
				Regula ==utiliz, Regula1=Regula),
				lista_float_int(Reguli,Reguli1).
			
			afis_reguli([]).
				
			afis_reguli([N|X]) :- 
				afis_regula(N),
				premisele(N),
				afis_reguli(X).

				afis_regula(N) :- 
					regula(N, premise(Lista_premise),
					concluzie(Scop,FC)),NN is integer(N),
					scrie_lista(['r',' =',NN]),
					scrie_lista(['cond', ' ', '[']),
					scrie_lista_premise(Lista_premise),
					scrie_lista([']']),
					transformare2(Scop,Scop_tr), % AICI
					append([' =', '>'],Scop_tr,L1),
					FC1 is integer(FC),append(L1,['avand fc','(',FC1,')'],LL),
					scrie_lista(LL),nl.

					scrie_lista_premise([]).
						
					scrie_lista_premise([H|T]) :- 
						transformare2(H,H_tr),
						space(5),
						write('#'),
						scrie_lista(H_tr),
						scrie_lista_premise(T).

				premisele(N) :- 
					regula(N, premise(Lista_premise), _),
					!, cum_premise(Lista_premise).

					cum_premise([]).
						
					cum_premise([Scop|X]) :- 
						cum(Scop),
						cum_premise(X).
			
		cum(_).
		
	executa([iesire]):- !.
		
	executa([_|_]) :- 
		write('Comanda incorecta! '),nl.