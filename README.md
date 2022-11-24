# commenter_pgn
Tool to add an evaluation for each move of each game of a PGN file

Prerequisites :<br>
rename BUREAU.ini to YOUR_COMPUTER_NAME.ini<br>
set moteur to path_to_your_engine.exe
set pgnextract = path_to_pgn-extract.exe
set mode to bestmove or searchmoves
set duree_sec to your analysis delay per move
set prof_fixe to your analysis fixed depth
set taches to your threads number
set memoire to your hash size
set priorite to 32 or 64 or 128 or 256 or 16384 or 32768

rename BrainLearn.txt to YOUR_ENGINE_NAME.txt<br>
set its UCI options<p>

command : commenter_pgn.exe path_to_your_pgn_file.pgn<p>

# How it works ?
There are different ways to use this tool :<br>
- with the bestmove mode, the engine will analyse the opponent's bestmove.<br>
I advise you to set a delay per move (ex : duree_sec = 600 for an analysis of 10 minutes per move) so you should set prof_fixe = 0.<p>

- with the searchmoves mode, the engine will only analyse the played move (maybe it was not the bestmove).<br>
I advise you to set a fixed depth (ex : prof_fixe = 40 for an analysis at D40 per move) so you should set duree_sec = 0.<p>

During the analyses, we get few files :<br>
- the "your_pgn_annotated.pgn" file contains the last game with score/depth for each move<br>
- the "your_pgn_uci.pgn" file contains the UCI moves (thanks to pgn-extract)<br>
- the "your_computer_name_reprise_searchmoves/bestmove.ini" file contains the data to resum the analyses<br>

# tips
For those who use engines with a learning feature, I advise you to analyze your defeats (and those of other engines) in bestmove mode with a long analysis time (eg: 10 min or more per move). In one go commenter_pgn will improve PGN file move rating + update experience file with better ratings + add best moves + save analyzed position to save time next time.<p>
