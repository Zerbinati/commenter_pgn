# commenter_pgn
Tool to add an evaluation for each move of each game of a PGN file

Prerequisites :<br>
rename BUREAU.ini to YOUR_COMPUTER_NAME.ini<br>
set engine to path_to_your_engine.exe<br>
set pgnextract = path_to_pgn-extract.exe<br>
set mode to bestmove or searchmoves<br>
set delay_sec to your analysis delay per move<br>
set fixed_depth to your analysis fixed depth<br>
set threads to your threads number<br>
set hash to your hash size<br>
set priorite to 32 or 64 or 128 or 256 or 16384 or 32768<br>

rename BrainLearn.txt to YOUR_ENGINE_NAME.txt<br>
set its UCI options<p>

command : commenter_pgn.exe path_to_your_pgn_file.pgn<p>

# How it works ?
There are different ways to use this tool :<br>
- with the bestmove mode, the engine will analyse the opponent's bestmove.<br>
I advise you to set a delay per move (ex : delay_sec = 60 for an analysis of 1 minute per move) so you should set fixed_depth = 0.<p>

- with the searchmoves mode, the engine will only analyse the played move.<br>
I advise you to set a fixed depth (ex : fixed_depth = 40 for an analysis at D40 per move) so you should set delay_sec = 0.<p>

During the analyses, we get few files :<br>
- the "your_pgn_annotated.pgn" file contains the last game with score/depth for each move<br>
- the "your_pgn_uci.pgn" file contains the UCI moves (thanks to pgn-extract)<br>
- the "your_computer_name_reprise_searchmoves/bestmove.ini" file contains the data to resume the analyses<br>
- in the "engine_eval" directory, the EPD files contain the fen, score, depth, delay of each unkown/new positions<p>

# tips
For those who use engines with a learning feature, i advise you :<br>
- to analyze your defeats (and those of other engines) in bestmove mode with a long analysis delay (ex: 10 min or more per move). In one time, commenter_pgn will show you when your engine made a bad move + what was the bestmove + update your experience file with better data for the played moves + new data containing the best moves + store the analyzed positions to save time on the next analyses.<p>
- to analyze openings in searchmoves with a great depth (ex: D40). In one time, commenter_pgn will update your experience file with better data for the opening moves + fill the holes in the experience data (the engine will get a complete line from the start position to the last position of a trained opening) + store the analyzed positions to save time on the next analyses.<p>
