:- dynamic i_am_at/1.
:- dynamic monster/3.
:- dynamic player_weapon_level/1.
:- retractall(i_am_at(_)).
:- consult(map).
:- consult(display_map).

respawn :-
    i_am_at(Here),
    retract(i_am_at(Here)),
    assert(i_am_at(d4)),
    write('You will need stronger weapon to defeat that monster!'), nl,
    write('Local medic has found your body and brang to Anyor!'), nl,
    look.

combat(Monster) :-
    % Your combat logic here
    write('You are battling '), write(Monster), nl,
    player_weapon_level(PlayerWeaponLevel),
    i_am_at(Loc),
    monster(Loc, Monster, MonsterLevel),
    (
        PlayerWeaponLevel >= MonsterLevel ->
        write('You have defeated the '), write(Monster), write(' with your weapon level '), write(PlayerWeaponLevel), write('!'), nl,
        retract(monster(Loc, Monster, MonsterLevel)),
        (   lvlup(Loc) ->
            retract(player_weapon_level(PlayerWeaponLevel)),
            NewLevel is PlayerWeaponLevel + 1,
            assert(player_weapon_level(NewLevel)),
            write('WEAPON UPGRADE FOUND!!!'), nl,
            write('Your weapon has been upgraded to level '), write(NewLevel), nl
        ;   true
        )
        % Add further logic for gaining experience or other rewards
        ;
        write('You were defeated by the '), write(Monster), nl,
        respawn
    ).

/* This rule just writes out game instructions. */
look :-
    i_am_at(Place),
    describe(Place),
    write('You are at '), write(Place), nl,
    (   monster(Place, Monster, Level) ->
    write('A wild '), write(Monster), write(' [lvl '), write(Level), write('] appears!'), nl,
    combat(Monster)
    ;   true
    ),
    nl.

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).


/* This rule tells how to move in a given direction. */

go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        !, look.

go(_) :-
        write('You can''t go that way.').

instructions :-
    nl,
    write('Enter commands using standard Prolog syntax.'), nl,
    write('Available commands are:'), nl,
    write('start.             -- to start the game.'), nl,
    write('n.  s.  e.  w.     -- to go in that direction.'), nl,
    write('look.              -- to look around you again.'), nl,
    write('instructions.      -- to see this message again.'), nl,
    write('halt.              -- to end the game and quit.'), nl,
    nl.


/* This rule prints out instructions and tells where you are. */

start :-
    assert(i_am_at(d4)),
    assert(player_weapon_level(1)),
    write('In a realm veiled by ancient lore, three guardians protected fragments of a mysterious key.'), nl,
    write('Druid guarded the forest, the Undead Priest watched over the temple, and a formidable Goblin held the key fragment in treacherous caves.'), nl,
    write('Legends whispered of an elusive entity, the Ephemeral Phantom, residing in the abandoned house, said to hold the power to shape the realm\'s destiny.'), nl, 
    write('Only a brave soul could confront this being and determine the realm\'s fate.'), nl,
    instructions,
    look,
    write('New quest added: Collect 3 key fragments.'), nl.