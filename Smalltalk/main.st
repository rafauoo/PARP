"The germ of a text adventure game
Marcin Szlenk 2022"

"Game class

Responsibilities:
* print/read outputs/inputs
* handle game loop
* handle 'instructions' and 'halt' commands

Collaborators:
* ..."

Object subclass: Game [
    | isGameOver currentLocation worldMap eq weaponLv |

    introductionText := #(
        'A long time ago, in a galaxy far, far away...'
    ).

    instructionsText := #(
        'Available commands are:'
        ''
        'instructions  -- to see these instructions.'
        'quit          -- to end the game and quit.'
        'bag           -- to see your equipment.'
        'w	       -- go west.'
        'n	       -- go north.'
        'e	       -- go east.'
        's	       -- go south.'
        ''
    ).
 
    Game class >> new [
        | g |
        g := super new.
        g init.
        ^g
    ]

    Game >> init [
        isGameOver := false.
	    currentLocation := 'd4'.
	    worldMap := GameMap new.
        eq := OrderedCollection new.
        weaponLv := 2.
    ]

    Game >> look [
	| description monster |
        monster := worldMap getMonster: currentLocation.
        description := 'You are in ', (worldMap getLocationDescription: currentLocation), ' (', currentLocation,  ')'.
        self printLines: (Array with: description).
        description := 'Your level: ', (weaponLv printString).
        self printLines: (Array with: description).
        monster = false ifFalse: [
            description := 'A wild ', (monster first), ' [lv ', (monster second printString), '] appears!!!'.
            self printLines: (Array with: description).
            self combat: monster on: currentLocation.
        ].
    ]

    Game >> dead [
        | description |
        description := #(
            'You have been slain'
            'Local medic has found your body...'
        ).
        self printLines: description.
        currentLocation := 'd4'.
        ^ self look.
    ]

    Game >> combat: monster on: location [
        | monsterlv description key lvUp|
        monsterlv := monster second.
        monsterlv > weaponLv ifTrue: [
            ^ self dead.
        ].
        worldMap deleteStaticMonster: location.
        key := self addKey: location.
        key = 'Not key' ifFalse: [
            description := #(
                'Key was founded. Check your bag.'
                ''
            ).
            self printLines: description.
        ].
    ]

    Game >> addKey: location [
        | key |
        key := worldMap getKey: location.
        key = 'Not key' ifFalse: [
            eq add: key.
        ].
        ^key.
    ]

    "Print strings from array in separate lines."
    Game >> printLines: anArray [
        anArray do: [ :string | Transcript show: string; cr ]
    ]

    Game >> printIntroduction [
       self printLines: introductionText.
    ]

    Game >> printInstructions [
        self printLines: instructionsText.
    ]

    Game >> readCommand [
        Transcript show: '> '.
        ^ FileStream stdin nextLine
    ]
    Game >> move: direction [
	| filteredConnections chosenConnection |
	filteredConnections := worldMap getConnectionsFrom: currentLocation.
	chosenConnection := (filteredConnections select: [:conn | conn second = direction]).

        chosenConnection isEmpty
            ifTrue: [
                Transcript show: 'You cannot go there!'; nl.
            ]
            ifFalse: [
                currentLocation := chosenConnection first third.
                self look.
            ].
    ]

    "Run the game."
    Game >> run [
        | cmd isUnknown |

        self printIntroduction.
        self printInstructions.
	    self look.
        "Game loop."
        [isGameOver] whileFalse: [
        isUnknown := true.
        cmd := self readCommand.

        cmd = 'instructions' ifTrue: [
            self printInstructions.
            isUnknown := false
        ].

        cmd = 'quit' ifTrue: [
            isGameOver := true.
            isUnknown := false
        ].
	
	    cmd = 'w' ifTrue: [
		self move: 'w'.
		isUnknown := false
	    ].
	    
	    cmd = 'n' ifTrue: [
		self move: 'n'.
		isUnknown := false
	    ].
	    
	    cmd = 's' ifTrue: [
		self move: 's'.
		isUnknown := false
	    ].

	    cmd = 'e' ifTrue: [
		self move: 'e'.
		isUnknown := false
	    ].

        cmd = 'bag' ifTrue: [
        self printLines: #('Your equipment:').
        eq do: [ :key |
            Transcript show: '  - ', key; cr.
        ].
        isUnknown := false
        ].

        isUnknown ifTrue: [
        self printLines: #('Unknown command.' '')
        ].
        ].
    ]
]

g := Game new.
g run.
