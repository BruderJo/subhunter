' subhunter game
' vers. 2020-08-14
'
' ------------------------------------------------------------
' !!!!!   W O R K   I N   P R O G R E S S   !!!!!
' This is not a function code
' The game is under development
' But I want to show the progress
' ------------------------------------------------------------
'
' you control a destroyer ship
' hit all hostile submarines before
' they hit you with their torpedos

' cursor key and fire
' move = left/right
' fire = space

' ------------------------------------------------------------
Option explicit     ' Variables must be defined first
Option Default none ' all variables must be specified

Option base 0       ' Array Index starts with 0 
' ------------------------------------------------------------

' global definition and constants

' how many objects can exist
Const MAXBOMB  =  5 ' Water bombs
Const MAXSUB   =  8 ' Submarines
Const MAXTORP  = 16 ' Torpedos

' 2nd array index. objects attributes
Const IDX_X    =  0 ' coordinate X
Const IDX_Y    =  1 ' coordinate Y
Const IDX_DX   =  2 ' speed X
Const IDX_DY   =  3 ' speed Y
Const IDX_COND =  4 ' condition (dead or alive)
Const IDX_CNT  =  5 ' visible effect counter/delay
Const IDX_SPR  =  6 ' object's sprite image
Const MAXIDX   =  6 ' the highest index above


' object conditions
Const COND_FREE     = 0 ' object is not active
Const COND_OK       = 1 ' object is alive
Const COND_HIT      = 2 ' object is hit
Const COND_REMOVE   = 3 ' object can be removed from screen


' all movable object
Dim integer obj_bomb(MAXBOMB,MAXIDX)
Dim integer obj_sub(MAXSUB,MAXIDX)
Dim integer obj_torp(MAXTORP,MAXIDX)
Dim integer obj_dest(MAXIDX)

Const SPRITE_NULL  = 0	 ' empty image
Const SPRITE_DESTR = 1   ' image Destoyer moves left->right
Const SPRITE_DESTL = 2   ' image Destoyer move right->left
Const SPRITE_SUBR  = 3   ' image submarine moves left->right
Const SPRITE_SUBL  = 4   ' image submarine move right->left
Const SPRITE_BOMB  = 5   ' image water bomb
Const SPRITE_TORP  = 6   ' image torpedo
Const SPRITE_EXPL  = 7   ' image explosion


' delay between each game loop
' timer value in ms
Const interval  = 100


' initialize game variables
'

' the game loop counter
' controls all activities
' increased after each 'interval'
Dim integer gloop

' trigger the start of new submarines
Dim integer trigger_sub         ' decrement each game loop
Dim integer trigger_sub_init    ' init value to start new counter
Const TRIGGER_SUB_MAX = 60	' global init value

' trigger the torpedo launch
Dim integer trigger_torp
Dim integer trigger_torp_init
Const TRIGGER_TORP_MAX = 100 	' global init value
Dim integer torpedos_fired

Dim integer bomb_fired

' what is the game's status?
' can be init, show about, game run, etc.
Dim integer game_status         ' the game status

' Const GSTATUS = ... hmm: new, init, about, running

Dim integer score, highscore    ' the score values

' Y-Size of graphic areas on screen
' assuming mode is 640x400
'
Dim integer MAXX, MAXY, MINX,MINY	' Graphic 640,400,0,0

Const GSCRN_STATUS_SIZE  = 24	' Status line, Text font 2 has 20px high
Const GSCRN_SURFACE_SIZE = 60	' surface area
Const GSCRN_BOTTOM_SIZE  = 16	' ground level
Const GSCRN_OCEAN_SIZE   = 300	' the ocean area (=400-24-60-16)

' Color set
Const GSCRN_STATUS_COL  = ORANGE
Const GSCRN_SURFACE_COL = WHITE
Const GSCRN_BOTTOM_COL  = BROWN
Const GSCRN_OCEAN_COL   = BLUE

' Start position of destroyer
Const DEST_STARTX	= 400
Const DEST_STARTY	= GSCRN_STATUS_SIZE+GSCRN_SURFACE_SIZE
const MAX_SPEED         = 3	' maximum speed of destroyer/subs

' Keyboard Definitions
' movement.
' 0 =  no move
' -1 = move left
'  1 = move right
Dim integer key_move	= 0
Dim integer key_fire	= 0
Dim integer key_current

' ------------------------------------------------------------
Sub do_initGrafix
  Mode 2,8	' Graphic Mode: 640 x 400 @8bit per pixel
  Colour white, GSCRN_OCEAN_COL
  CLS
End Sub
' ------------------------------------------------------------

' ------------------------------------------------------------

' ------------------------------------------------------------


' ------------------------------------------------------------
' Initialize all game object attributes
' some are initialized when object becomes active
'
'   ----------------------------------------------------------
Sub do_ResetObjects
'   ----------------------------------------------------------
local integer i
local integer dx
local integer yy

  obj_dest(IDX_X) = DEST_STARTX
  obj_dest(IDX_Y) = DEST_STARTY
  obj_dest(IDX_DX) = 0
  obj_dest(IDX_COND) = COND_OK
  obj_dest(IDX_SPR) = SPRITE_DESTR

  for i=0 to MAX_BOMB
    obj_bomb(i,IDX_COND) = COND_FREE
    obj_bomb(i,IDX_SPR)  = SPRITE_BOMB
  next i
  for i=0 to MAX_SUB
    obj_sub(i,IDX_COND) = COND_FREE
    ' each sub has it's own depth -> do not collide
    yy = GSCRN_OCEAN_SIZE / (MAX_SUB + 3) ' add 3: 
    obj_sub(i,IDX_Y) = (i + 2) * yy

    'little fun: subs are faster, if depth is lower
    dx = 1
    if (i < MAX_SUB/3)       THEN dx = 3
    if (i < (2 * MAX_SUB)/3) THEN dx = 2
    obj_sub(i,IDX_DX) = dx

    ' left/right
    if ((i MOD 2) = 0) THEN 
      obj_sub(i,IDX_DX) = dx
    else
      obj_sub(i,IDX_DX) = -dx
    endif
  next i

  for i=0 to MAX_TORP
    obj_torp(i,MAX_TORP) = COND_FREE
  next i

  trigger_sub_init = TRIGGER_SUB_MAX
  trigger_sub      = trigger_sub_init / 4	' just to present the first sub earlier

  trigger_torp_init = TRIGGER_TORP_MAX
  trigger_torp      = TRIGGER_TORP_MAX

  gloop = 0
  torpedo_fired = 0
  bomb_fired = 0
End Sub


' ------------------------------------------------------------
' Keyboard keys trigger actions
' key left/right add speed vector up to max
'   ----------------------------------------------------------
Sub do_KeyLeft
'   ----------------------------------------------------------
  obj_dest(IDX_DX) = obj_dest(IDX_DX) - 1
  if (obj_dest(IDX_DX) < -MAX_SPEED) then obj_dest(IDX_DX) = -MAX_SPEED
End Sub

'   ----------------------------------------------------------
Sub do_KeyRight
'   ----------------------------------------------------------
  obj_dest(IDX_DX) = obj_dest(IDX_DX) + 1
  if (obj_dest(IDX_DX) > MAX_SPEED) then obj_dest(IDX_DX) = MAX_SPEED
End Sub

'   ----------------------------------------------------------
Sub do_KeyFire
'   ----------------------------------------------------------
  bomb_fired = 1
End Sub
' ------------------------------------------------------------


' ------------------------------------------------------------
' calculate a random value between 0..i
'   ----------------------------------------------------------
Function do_Random (n%) as integer
'   ----------------------------------------------------------
  local integer i
  i = rnd * 256;
  do_Random = i * n% / 256
end Function
' ------------------------------------------------------------

'   ----------------------------------------------------------
Sub do_initGame
'   ----------------------------------------------------------
  do_initGrafix
  do_ResetObjects

  torpedos_fired = 0
  score = 0
  highscore = 0

End Sub
' ------------------------------------------------------------

' ------------------------------------------------------------
' Paint
' Draw the screen with all objects
'
'   ----------------------------------------------------------
Sub do_Paint
'   ----------------------------------------------------------

End Sub
' ------------------------------------------------------------

' ------------------------------------------------------------
Sub do_activate_sub
  local integer i

  trigger_sub = trigger_sub - 1
  if (trigger_sub < 0) then	' threshold, launch new submarine
    trigger_sub = trigger_sub_init
    i = do_Random(MAX_SUB)	' choose a random slot
    if (obj_sub(i,IDX_COND) = COND_FREE) then	' slot is free ..
      obj_sub(i,IDX_COND) = COND_OK
      ' now assign start position
      if (obj_sub(i,IDX_DX) < 0) THEN	' sub moves right->left
        obj_sub(i,IDX_X) = MAXX
        obj_sub(i,IDX_SPR) = SPRITE_SUBL
      else				' sub moves left->right
        obj_sub(i,IDX_X) = MINX
        obj_sub(i,IDX_SPR) = SPRITE_SUBR
      end if
    end if
  endif

End Sub

' ------------------------------------------------------------
Sub do_moveSubmarine
End Sub

' ------------------------------------------------------------
Sub do_activateTorpedo
End Sub

' ------------------------------------------------------------
Sub do_moveTorpedo
End Sub

' ------------------------------------------------------------
Sub do_activateBomb
End Sub

' ------------------------------------------------------------
Sub do_moveBomb
End Sub



' ------------------------------------------------------------
Sub do_moveDestroyer

  if (obj_dest(IDX_COND)=COND_OK) then
    if (obj_dest(IDX_DX) < 0) then ' move left
      obj_dest(IDX_SPR) = SPRITE_DESTL
      obj_dest(IDX_X) = obj_dest(IDX_X) + obj_dest(IDX_DX)
      if (obj_dest(IDX_X) < MINX) then obj_dest(IDX_X) = MINX
    else
    if (obj_dest(IDX_DX) > 0) then ' move right
      obj_dest(IDX_SPR) = SPRITE_DESTR
      obj_dest(IDX_X) = obj_dest(IDX_X) + obj_dest(IDX_DX)
      if (obj_dest(IDX_X) > MAXX) then obj_dest(IDX_X) = MAXX
    end if
    if (bomb_fired = 1) then
      do_activateBomb
    end if
  else
    if (obj_dest(IDX_COND) = COND_HIT then
      obj_dest(IDX_SPR) = SPRITE_EXPL
      obj_dest(IDX_CNT) = obj_dest(IDX_CNT) - 1
      if (obj_dest(IDX_CNT) < 0) then
        obj_dest(IDX_COND) = COND_FREE;
        game_status = GAME_ABOUT
      end if
  end if
End Sub
' ------------------------------------------------------------


' ------------------------------------------------------------
' Main Game Loop
' the tiny automat
'   ----------------------------------------------------------
Sub do_game
'   ----------------------------------------------------------
  game main functions
  do_Paint
End Sub
' ------------------------------------------------------------
