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
    | isGameOver currentLocation worldMap eq|

    introductionText := #(
        'A long time ago, in a galaxy far, far away...'
    ).

    instructionsText := #(
        'Available commands are:'
        ''
        'instructions  -- to see these instructions.'
        'quit          -- to end the game and quit.'
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

    init [
        isGameOver := false.
	    currentLocation := 'd4'.
	    worldMap := GameMap new.
        eq := OrderedCollection new.
    ]

    look [
	| locationDescription monster |
        monster := worldMap getMonster: currentLocation.
        monster = false ifFalse: [
            Transcript show: monster first, ' ', monster second printString; cr.
        ].
        locationDescription := 'You are in ', (worldMap getLocationDescription: currentLocation) , ' (' , currentLocation , ')'.
        self printLines: (Array with: locationDescription).
    ]

    addKey: location [
        | key |
        key := worldMap getKey: location.
        key = 'Not key' ifFalse: [
            eq add: key.
        ].
    ]

    "Print strings from array in separate lines."
    printLines: anArray [
        anArray do: [ :string | Transcript show: string; cr ]
    ]

    printIntroduction [
       self printLines: introductionText.
    ]

    printInstructions [
        self printLines: instructionsText.
    ]

    readCommand [
        Transcript show: '> '.
        ^ FileStream stdin nextLine
    ]
    move: direction [
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
    run [
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
            Transcript show: '   -', key; cr.
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
