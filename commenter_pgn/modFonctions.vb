Imports System.Management 'ajouter référence system.management
Imports VB = Microsoft.VisualBasic

Module modFonctions
    Public processus As System.Diagnostics.Process
    Public entree As System.IO.StreamWriter
    Public sortie As System.IO.StreamReader

    Public Function analyseCoups(coupsEN As String, pgnFR As String, Optional epd As String = "") As String
        Dim tabCoups() As String, i As Integer, pieceCoups As String, separateur As String, promotion As String
        Dim chaine As String, departCoups As String, arriveeCoups As String, partie As String
        Dim departChaine As String, arriveeChaine As String, pieceChaine As String, chaineCoups As String
        Dim casesOccupees As String, prisePassant As String, coups As String, prefixFR As String

        chaineCoups = ""
        casesOccupees = "a1;b1;c1;d1;e1;f1;g1;h1;a2;b2;c2;d2;e2;f2;g2;h2;a7;b7;c7;d7;e7;f7;g7;h7;a8;b8;c8;d8;e8;f8;g8;h8;"
        prefixFR = "Ta1-a1 Cb1-b1 Fc1-c1 Dd1-d1 Re1-e1 Ff1-f1 Cg1-g1 Th1-h1 a2-a2 b2-b2 c2-c2 d2-d2 e2-e2 f2-f2 g2-g2 h2-h2 a7-a7 b7-b7 c7-c7 d7-d7 e7-e7 f7-f7 g7-g7 h7-h7 Ta8-a8 Cb8-b8 Fc8-c8 Dd8-d8 Re8-e8 Ff8-f8 Cg8-g8 Th8-h8 "
        If epd <> "" Then
            tabCoups = Split(epdCasesOccupees(epd), ":")
            casesOccupees = tabCoups(0)
            prefixFR = tabCoups(1)
        End If
        prisePassant = "a5xb6 b5xa6 b5xc6 c5xb6 c5xd6 d5xc6 d5xe6 e5xd6 e5xf6 f5xe6 f5xg6 g5xf6 g5xh6 h5xg6"
        prisePassant = prisePassant & " " & "a4xb3 b4xa3 b4xc3 c4xb3 c4xd3 d4xc3 d4xe3 e4xd3 e4xf3 f4xe3 f4xg3 g4xf3 g4xh3 h4xg3"
        pieceCoups = ""
        separateur = ""
        departCoups = ""
        arriveeCoups = ""
        departChaine = ""
        arriveeChaine = ""
        pieceChaine = ""
        promotion = ""

        'on dégage les commentaires
        partie = formaterCoups("", prefixFR & pgnFR)

        'on convertit les roques
        If InStr(partie, ". 0-0 ", CompareMethod.Text) > 0 Then
            partie = Replace(partie, ". 0-0 ", ". Re1-g1 Th1-f1 ")
        ElseIf InStr(partie, ". 0-0-0 ", CompareMethod.Text) > 0 Then
            partie = Replace(partie, ". 0-0-0 ", ". Re1-c1 Ta1-d1 ")
        End If
        If InStr(partie, " 0-0 ", CompareMethod.Text) > 0 Then
            partie = Replace(partie, " 0-0 ", " Re8-g8 Th8-f8 ")
        ElseIf InStr(partie, " 0-0-0 ", CompareMethod.Text) > 0 Then
            partie = Replace(partie, " 0-0-0 ", " Re8-c8 Ta8-d8 ")
        End If

        'on convertit les promotions
        'h2-h4 f7-f6 h4-h5 f6-f5 h5-h6 f5-f4 h6xg7 f4-f3 g7xh8=D Ff8-g7
        'g7xh8=D => g7xh8 + Dh8-h8
        While InStr(partie, "=", CompareMethod.Text) > 0
            partie = Replace(partie, partie.Substring(partie.IndexOf("=") - 2, 4), partie.Substring(partie.IndexOf("=") - 2, 2) & " " & partie.Substring(partie.IndexOf("=") + 1, 1) & partie.Substring(partie.IndexOf("=") - 2, 2) & "-" & partie.Substring(partie.IndexOf("=") - 2, 2))
            Threading.Thread.Sleep(1)
        End While

        'on cherche les infos du coups
        coups = formaterCoups("moteur", coupsEN)
        departCoups = gauche(coups, 2)
        If Len(coups) < 5 Then
            arriveeCoups = droite(coups, 2)
        Else
            Select Case droite(coups, 1)
                Case "q", "Q"
                    promotion = "=D"
                Case "r", "R"
                    promotion = "=T"
                Case "b", "B"
                    promotion = "=F"
                Case "n", "N"
                    promotion = "=C"
            End Select
            arriveeCoups = droite(gauche(coups, 4), 2)
        End If

        'on met à jour les cases occupées
        tabCoups = Split(partie, " ")
        For i = 0 To UBound(tabCoups)
            If tabCoups(i) <> "" And InStr(tabCoups(i), ".", CompareMethod.Text) = 0 And InStr(tabCoups(i), "{", CompareMethod.Text) = 0 And InStr(tabCoups(i), "}", CompareMethod.Text) = 0 And InStr(tabCoups(i), "/", CompareMethod.Text) = 0 _
            And InStr(tabCoups(i), "*", CompareMethod.Text) = 0 And InStr(tabCoups(i), "1-0", CompareMethod.Text) = 0 And InStr(tabCoups(i), "0-1", CompareMethod.Text) = 0 And InStr(tabCoups(i), "1/2-1/2", CompareMethod.Text) = 0 Then
                'si on a une prise en passant
                If InStr(tabCoups(i), "ep", CompareMethod.Text) > 0 Then
                    'on efface la case d'arrivée du coups précédent (pion qui avance de 2 cases)
                    casesOccupees = Replace(casesOccupees, arriveeChaine & ";", "")
                End If
                chaine = formaterCoups("moteur", tabCoups(i))

                departChaine = gauche(chaine, 2)
                arriveeChaine = droite(chaine, 2)

                casesOccupees = Replace(casesOccupees, departChaine & ";", "")
                If InStr(casesOccupees, arriveeChaine & ";") = 0 Then
                    casesOccupees = casesOccupees & arriveeChaine & ";"
                End If

                casesOccupees = trierChaine(casesOccupees, ";") & ";"

            End If
            Threading.Thread.Sleep(1)
        Next

        departChaine = ""
        arriveeChaine = ""

        'on analyse le coups
        For i = UBound(tabCoups) To 0 Step -1
            If tabCoups(i) <> "" And InStr(tabCoups(i), ".", CompareMethod.Text) = 0 And InStr(tabCoups(i), "{", CompareMethod.Text) = 0 And InStr(tabCoups(i), "}", CompareMethod.Text) = 0 And InStr(tabCoups(i), "/", CompareMethod.Text) = 0 _
            And InStr(tabCoups(i), "*", CompareMethod.Text) = 0 And InStr(tabCoups(i), "1-0", CompareMethod.Text) = 0 And InStr(tabCoups(i), "0-1", CompareMethod.Text) = 0 And InStr(tabCoups(i), "1/2-1/2", CompareMethod.Text) = 0 Then
                pieceChaine = ""

                chaine = tabCoups(i)
                If Len(tabCoups(i)) = 6 Then
                    chaine = droite(tabCoups(i), 5)
                    pieceChaine = gauche(tabCoups(i), 1)
                End If

                departChaine = gauche(chaine, 2)
                arriveeChaine = droite(chaine, 2)

                'case de départ connue ?
                If pieceCoups = "" And departCoups = arriveeChaine Then
                    If pieceChaine = "" Then
                        pieceCoups = "p"
                    Else
                        pieceCoups = pieceChaine
                    End If
                End If

                'case d'arrivée connue ?
                If separateur = "" And InStr(casesOccupees, arriveeCoups & ";") > 0 Then
                    separateur = "x"
                End If
            End If
        Next

        If separateur = "" Then
            separateur = "-"
        End If

        chaineCoups = Replace(pieceCoups, "p", "") & departCoups & separateur & arriveeCoups & promotion

        'roque
        If chaineCoups = "Re1-g1" Or chaineCoups = "Re8-g8" Then
            chaineCoups = "0-0"
        ElseIf chaineCoups = "Re1-c1" Or chaineCoups = "Re8-c8" Then
            chaineCoups = "0-0-0"
        End If

        'prise en passant
        If InStr(prisePassant, Replace(chaineCoups, "-", "x"), CompareMethod.Text) > 0 Then
            chaineCoups = Replace(chaineCoups, "-", "x")
        End If

        Return chaineCoups
    End Function

    Public Function analyseSuite(suiteUCI As String, sec As Integer, prof As Integer, Optional startpos As String = "") As String
        Dim chaine As String, chaine_mem As String, chainePosition As String, chaineAnalyse As String
        Dim secAnalyse As Integer, profAnalyse As Integer

        chaine = ""
        chaine_mem = ""
        chainePosition = ""
        chaineAnalyse = ""
        secAnalyse = 0
        profAnalyse = 0

        'bestmove = on simule le coup, on analyse le meilleur coup adverse, on inversera le score
        If mode = "bestmove" Then
            If startpos = "" Then
                chainePosition = "position startpos moves " & suiteUCI
            Else
                chainePosition = "position fen " & startpos & " moves " & suiteUCI
            End If

            If sec > 0 Then
                chaineAnalyse = "go movetime " & sec * 1000
            ElseIf prof > 0 Then
                chaineAnalyse = "go depth " & prof
            End If

        ElseIf mode = "searchmoves" Then 'searchmoves : on limite la recherche au coup à analyser
            If startpos = "" Then
                If InStr(suiteUCI, " ") = 0 Then
                    chainePosition = "position startpos"
                Else
                    chainePosition = "position startpos moves " & suiteUCI.Substring(0, suiteUCI.LastIndexOf(" "))
                End If
            Else
                If InStr(suiteUCI, " ") = 0 Then
                    chainePosition = "position fen " & startpos
                Else
                    chainePosition = "position fen " & startpos & " moves " & suiteUCI.Substring(0, suiteUCI.LastIndexOf(" "))
                End If
            End If

            If sec > 0 Then
                chaineAnalyse = "go movetime " & sec * 1000 & " searchmoves " & suiteUCI.Substring(suiteUCI.LastIndexOf(" ") + 1)
            ElseIf prof > 0 Then
                chaineAnalyse = "go depth " & prof & " searchmoves " & suiteUCI.Substring(suiteUCI.LastIndexOf(" ") + 1)
            End If

            'position/coup déjà dans le fichierEXP ?
            If prof > 0 And (InStr(moteur_court, "eman", CompareMethod.Text) > 0 Or InStr(moteur_court, "hypnos", CompareMethod.Text) > 0 Or InStr(moteur_court, "stockfishmz", CompareMethod.Text) > 0) Then
                chaine_mem = retourExperience(chainePosition, suiteUCI.Substring(suiteUCI.LastIndexOf(" ") + 1), prof)
            End If
        End If

        If chaine_mem = "" Then
            entree.WriteLine(chainePosition)

            entree.WriteLine("isready")
            chaine = ""
            While InStr(chaine, "readyok") = 0
                chaine = sortie.ReadLine
                Threading.Thread.Sleep(1)
            End While

            entree.WriteLine(chaineAnalyse)

            While InStr(chaine, "bestmove") = 0
                If InStr(chaine, " depth ") > 0 And InStr(chaine, " score ") > 0 Then
                    If InStr(chaine, " pv ") > 0 Then
                        chaine_mem = chaine
                    Else
                        If chaine = "info depth 0 score mate 0" Then
                            chaine_mem = "checkmated"
                        ElseIf chaine = "info depth 0 score cp 0" Then
                            chaine_mem = "stalemated"
                        End If
                    End If
                    If InStr(chaine, "upper", CompareMethod.Text) = 0 And InStr(chaine, "lower", CompareMethod.Text) = 0 Then
                        prof_actuelle = chaine.Substring(chaine.IndexOf(" depth ") + 7)
                        profAnalyse = CInt(prof_actuelle.Substring(0, prof_actuelle.IndexOf(" ")))
                        prof_actuelle = "P" & Format(profAnalyse, "0")
                    ElseIf InStr(chaine, "upper", CompareMethod.Text) > 0 Then
                        prof_actuelle = chaine.Substring(chaine.IndexOf(" depth ") + 7)
                        profAnalyse = CInt(prof_actuelle.Substring(0, prof_actuelle.IndexOf(" ")))
                        prof_actuelle = "P" & Format(profAnalyse, "0") & "+"
                    ElseIf InStr(chaine, "lower", CompareMethod.Text) > 0 Then
                        prof_actuelle = chaine.Substring(chaine.IndexOf(" depth ") + 7)
                        profAnalyse = CInt(prof_actuelle.Substring(0, prof_actuelle.IndexOf(" ")))
                        prof_actuelle = "P" & Format(profAnalyse, "0") & "-"
                    End If
                    If InStr(chaine, "time", CompareMethod.Text) > 0 Then
                        ecoule_actuel = chaine.Substring(chaine.IndexOf(" time ") + 6)
                        secAnalyse = CInt(ecoule_actuel.Substring(0, ecoule_actuel.IndexOf(" "))) / 1000
                        ecoule_actuel = Format(secAnalyse, "0 sec")
                        If 1800 <= secAnalyse And prof <= 40 And (prof / 2) <= profAnalyse Then 'pour éviter les analyses interminables quand le score dépasse la plage d'évaluation du réseau
                            entree.WriteLine("stop")
                        End If
                    End If
                End If

                chaine = sortie.ReadLine
                Threading.Thread.Sleep(1)
            End While
        End If

        Return chaine_mem

    End Function

    Public Function binToMove(chaineBIN As String) As String
        Dim move As String

        '0 111 110 000 111 001
        '  Q   7   a   8   b

        move = ""

        Select Case gauche(droite(chaineBIN, 9), 3) 'a
            Case "000"
                move = move & "a"

            Case "001"
                move = move & "b"

            Case "010"
                move = move & "c"

            Case "011"
                move = move & "d"

            Case "100"
                move = move & "e"

            Case "101"
                move = move & "f"

            Case "110"
                move = move & "g"

            Case "111"
                move = move & "h"

        End Select

        Select Case gauche(droite(chaineBIN, 12), 3) '7
            Case "000"
                move = move & "1"

            Case "001"
                move = move & "2"

            Case "010"
                move = move & "3"

            Case "011"
                move = move & "4"

            Case "100"
                move = move & "5"

            Case "101"
                move = move & "6"

            Case "110"
                move = move & "7"

            Case "111"
                move = move & "8"

        End Select

        Select Case droite(chaineBIN, 3) 'b
            Case "000"
                move = move & "a"

            Case "001"
                move = move & "b"

            Case "010"
                move = move & "c"

            Case "011"
                move = move & "d"

            Case "100"
                move = move & "e"

            Case "101"
                move = move & "f"

            Case "110"
                move = move & "g"

            Case "111"
                move = move & "h"

        End Select

        Select Case gauche(droite(chaineBIN, 6), 3) '8
            Case "000"
                move = move & "1"

            Case "001"
                move = move & "2"

            Case "010"
                move = move & "3"

            Case "011"
                move = move & "4"

            Case "100"
                move = move & "5"

            Case "101"
                move = move & "6"

            Case "110"
                move = move & "7"

            Case "111"
                move = move & "8"

        End Select


        Select Case droite(gauche(chaineBIN, 4), 3) 'Q
            Case "001"
                move = move & "N"

            Case "101"
                move = move & "B"

            Case "110"
                move = move & "R"

            Case "111"
                move = move & "Q"

        End Select

        'grand roque blanc ou noir
        If gauche(chaineBIN, 4) = "1100" Then
            If move = "e1a1" Or move = "e8a8" Then
                move = Replace(move, "a", "c")
            End If
        End If

        Return move
    End Function

    Public Function brainlearn_expListe(fichierBIN As String, keyUCI As String) As String
        Dim tabTampon(23) As Byte, chaineExperience As String, compteur As Integer
        Dim tabBIN(0) As Byte, pos As Long, i As Integer, j As Integer
        Dim tabCoups(1000) As String, tabProfondeurs(1000) As Integer, tabScores(1000) As Integer, tabPerformances(1000) As Integer
        Dim coup As String, profondeur As Integer, scoreCP As String, scoreMAT As String, performance As Integer
        Dim keyBIN As String
        Dim lectureBIN As IO.FileStream, posLecture As Long, tailleBIN As Long, tailleTampon As Long, reservation As Boolean

        posLecture = 0
        tailleBIN = FileLen(fichierBIN)
        tailleTampon = tailleBIN
        i = 50
        lectureBIN = New IO.FileStream(fichierBIN, IO.FileMode.Open, IO.FileAccess.Read, IO.FileShare.ReadWrite)

        compteur = 1
        While posLecture < tailleBIN
            If posLecture + tailleTampon <= tailleBIN Then
                reservation = False
                Do
                    Try
                        ReDim tabBIN(tailleTampon - 1)
                        reservation = True
                    Catch ex As Exception
                        i = i - 1
                        tailleTampon = 24 * i * 1000000
                    End Try
                Loop Until reservation
                lectureBIN.Read(tabBIN, 0, tabBIN.Length)
            Else
                tailleTampon = tailleBIN - posLecture
                ReDim tabBIN(tailleTampon - 1)
                lectureBIN.Read(tabBIN, 0, tabBIN.Length)
            End If

            pos = 0
            Do
                Array.Copy(tabBIN, pos, tabTampon, 0, 24) 'clé inversée
                keyBIN = ""
                For i = 0 To 7
                    keyBIN = keyBIN & hexa(tabTampon(i))
                Next

                If keyUCI = keyBIN Then
                    'inversed key (8 octets) | Depth |          | score       | inv. move |       | Performance |
                    'h0 h1 h2 h3 h4 h5 h6 h7 | h8    | h9 hA hB | hC hD hE hF | h0 h1     | h2 h3 | h4          | h5 h6 h7 (24 octets)
                    '00 01 02 03 04 05 06 07   08      09 10 11   12 13 14 15   16 17       18 19   20            21 22 23 tabTampon
                    '-----------------------------------------------------------------------------------------------------
                    'fb 59 2f 56 d4 01 8f 8f | 1e    | 00 00 00 | 3f 00 00 00 | 1c 03     | 00 00 | 64          | 00 00 00
                    'decimal                   30                 0000003f      031c                100%
                    '										      63/208        001 100 011 100
                    '										      +0.30         2   e   4   e
                    '                                                           e2e4

                    coup = hexa(tabTampon(17)) & hexa(tabTampon(16))
                    '031c => 001 100 011 100 => e2e4
                    coup = binToMove(hexadecimalToBinaire(coup))

                    profondeur = tabTampon(8)

                    scoreCP = hexa(tabTampon(15)) & hexa(tabTampon(14)) & hexa(tabTampon(13)) & hexa(tabTampon(12))
                    scoreMAT = scoreCP
                    'score positif
                    If tabTampon(15) = 0 And tabTampon(14) = 0 Then
                        If hexa(tabTampon(13)) = "7D" And hexa(tabTampon(12)) = "02" Then
                            scoreCP = "<empty>"
                        ElseIf hexa(tabTampon(13)) = "7C" Then
                            scoreMAT = "+" & Format((32001 - Convert.ToInt64(scoreMAT, 16)) / 2, "0")
                            scoreCP = ""
                        Else
                            scoreCP = Format(Convert.ToInt64(scoreCP, 16) / 2.08, "0")
                        End If
                    ElseIf tabTampon(15) = 255 And tabTampon(14) = 255 Then
                        If hexa(tabTampon(13)) = "83" Then
                            scoreMAT = "-" & Format((32000 - (4294967296 - Convert.ToInt64(scoreMAT, 16))) / 2, "0")
                            scoreCP = ""
                        Else
                            scoreCP = Format((Convert.ToInt64(scoreCP, 16) - 4294967295) / 2.08, "0")
                        End If
                    Else
                        MsgBox("scoreCP = " & scoreCP, MsgBoxStyle.Exclamation, "en travaux")
                    End If

                    performance = tabTampon(20)

                    tabCoups(compteur) = coup
                    tabProfondeurs(compteur) = profondeur
                    If scoreCP <> "" Then
                        tabScores(compteur) = CInt(Replace(scoreCP, "<empty>", "0"))
                    Else
                        tabScores(compteur) = CInt(scoreMAT) * 1000
                    End If
                    tabPerformances(compteur) = performance
                    '1 : e2e4 , depth: 30, eval: cp 30, quality: 100      

                    compteur = compteur + 1
                End If

                pos = pos + 24
            Loop While pos < tabBIN.Length

            posLecture = lectureBIN.Position
            tabBIN = Nothing
        End While
        lectureBIN.Close()
        compteur = compteur - 1

        'on classe les scores
        For i = 1 To compteur
            For j = 1 To compteur
                If tabScores(j) < tabScores(i) _
                Or (tabScores(j) = tabScores(i) And tabProfondeurs(j) < tabProfondeurs(i)) Then
                    coup = tabCoups(i)
                    tabCoups(i) = tabCoups(j)
                    tabCoups(j) = coup

                    profondeur = tabProfondeurs(i)
                    tabProfondeurs(i) = tabProfondeurs(j)
                    tabProfondeurs(j) = profondeur

                    pos = tabScores(i)
                    tabScores(i) = tabScores(j)
                    tabScores(j) = pos

                    performance = tabPerformances(i)
                    tabPerformances(i) = tabPerformances(j)
                    tabPerformances(j) = performance
                End If
            Next
        Next

        'on rassemble les infos
        chaineExperience = ""
        For i = 1 To compteur
            chaineExperience = chaineExperience & i & " : " & tabCoups(i) & ", depth: " & tabProfondeurs(i)
            If tabScores(i) <= -1000 Or 1000 <= tabScores(i) Then
                chaineExperience = chaineExperience & ", eval: mate " & Format(tabScores(i) / 1000, "0")
            Else
                chaineExperience = chaineExperience & ", eval: cp " & tabScores(i)
            End If

            chaineExperience = chaineExperience & ", quality: " & tabPerformances(i) & vbCrLf
        Next

        Return chaineExperience

    End Function

    Public Function brainlearnQuality(cheminEXE As String, cheminBIN As String, suiteUCI As String, suiteFR As String, Optional startpos As String = "") As String
        Dim chaine As String, tabChaine() As String, keyUCI As String, keyBIN As String, positionEPD As String, mat As String, score As Single

        'suiteUCI/startpos => keyUCI
        If InStr(suiteUCI, " ") = 0 Then
            If startpos = "" Then
                keyUCI = uciKEY(entree, sortie, "")
                positionEPD = moteurEPD(cheminEXE, "")
            Else
                keyUCI = uciKEY(entree, sortie, "", startpos)
                positionEPD = moteurEPD(cheminEXE, "", startpos)
            End If
        Else
            If startpos = "" Then
                keyUCI = uciKEY(entree, sortie, suiteUCI.Substring(0, suiteUCI.LastIndexOf(" ")))
                positionEPD = moteurEPD(cheminEXE, suiteUCI.Substring(0, suiteUCI.LastIndexOf(" ")))
            Else
                keyUCI = uciKEY(entree, sortie, suiteUCI.Substring(0, suiteUCI.LastIndexOf(" ")), startpos)
                positionEPD = moteurEPD(cheminEXE, suiteUCI.Substring(0, suiteUCI.LastIndexOf(" ")), startpos)
            End If
        End If

        'clé normale => inversée
        keyBIN = ""
        For j = 0 To 15 Step 2
            chaine = keyUCI.Substring(j, 2)
            If Len(chaine) = 1 Then
                chaine = "0" & chaine
            End If
            keyBIN = chaine & keyBIN
        Next
        keyUCI = keyBIN
        keyBIN = ""

        'keyBIN => liste expérience
        chaine = brainlearn_expListe(cheminBIN, keyUCI)

        '1 : e2e4 , depth: 30, eval: cp 30, quality: 100
        If InStr(chaine, vbCrLf) > 0 Then
            chaine = gauche(chaine, chaine.IndexOf(vbCrLf))
        End If

        If InStr(chaine, "<empty>", CompareMethod.Text) > 0 Then
            chaine = ""
        End If

        If chaine <> "" Then
            chaine = Replace(chaine, "  ", "")
            chaine = Replace(chaine, ",", ":")
            chaine = Replace(chaine, ": ", ":")
            chaine = Replace(chaine, " :", ":")

            tabChaine = Split(chaine, ":")

            mat = "mate "
            If InStr(tabChaine(5), mat) = 0 Then
                score = CSng(CInt(Trim(Replace(tabChaine(5), "cp ", ""))) / 100)
                mat = ""
            Else
                score = CInt(Trim(Replace(tabChaine(5), mat, "")))
            End If

            If suiteUCI.Split(" ").Length Mod 2 = 0 Then
                score = -score
            End If

            tabChaine(1) = analyseCoups(tabChaine(1), suiteFR, positionEPD)

            chaine = StrDup(7 - Len(tabChaine(1)), " ") & tabChaine(1)

            If mat <> "" Then
                chaine = chaine & " {" & mat & Format(score, "0") & "/" & tabChaine(3) & "}"
                chaine = Replace(chaine, "{mate -", "{-M")
                chaine = Replace(chaine, "{mate ", "{+M")
            Else
                If score > 0 Then
                    chaine = chaine & " {+" & Format(score, "0.00") & "/" & tabChaine(3) & "}"
                ElseIf score = 0 Then
                    chaine = chaine & " { 0.00/" & tabChaine(3) & "}"
                Else
                    chaine = chaine & " {" & Format(score, "0.00") & "/" & tabChaine(3) & "}" 'score contient le "-"
                End If
            End If
        End If

        Return Replace(chaine, ",", ".")
    End Function

    Public Sub chargerMoteur(chemin As String, threads As Integer, hash As Integer, priorite As Integer)
        Dim chaine As String, fichierOptions As String, tabChaine() As String

        processus = New System.Diagnostics.Process()

        processus.StartInfo.RedirectStandardOutput = True
        processus.StartInfo.UseShellExecute = False
        processus.StartInfo.RedirectStandardInput = True
        processus.StartInfo.CreateNoWindow = True
        processus.StartInfo.WorkingDirectory = My.Application.Info.DirectoryPath
        processus.StartInfo.FileName = chemin
        processus.Start()
        processus.PriorityClass = priorite '64 (idle), 16384 (below normal), 32 (normal), 32768 (above normal), 128 (high), 256 (realtime)

        entree = processus.StandardInput
        sortie = processus.StandardOutput

        entree.WriteLine("uci")
        chaine = ""
        While InStr(chaine, "uciok") = 0
            chaine = sortie.ReadLine
            Threading.Thread.Sleep(1)
        End While

        'options communes
        entree.WriteLine("setoption name threads value " & threads)
        entree.WriteLine("setoption name hash value " & hash)
        entree.WriteLine("setoption name Ponder value false")
        entree.WriteLine("setoption name MultiPV value 1")

        'options spécifiques
        fichierOptions = Replace(nomFichier(chemin), ".exe", ".txt")
        If My.Computer.FileSystem.FileExists(fichierOptions) Then
            chaine = My.Computer.FileSystem.ReadAllText(fichierOptions)
            tabChaine = Split(chaine, vbCrLf)
            For i = 0 To UBound(tabChaine)
                If tabChaine(i) <> "" Then
                    entree.WriteLine(tabChaine(i))
                    If InStr(tabChaine(i), ".exp") > 0 Then

                        fichierEXP = Trim(tabChaine(i).Substring(tabChaine(i).IndexOf(" value ") + 7))
                        If InStr(fichierEXP, ":") = 0 Or InStr(fichierEXP, "\") = 0 Then
                            fichierEXP = Replace(chemin, nomFichier(chemin), nomFichier(fichierEXP))
                        End If

                        moteurEntete = ""
                        chaine = ""
                        While moteurEntete = "" And InStr(chaine, "not open", CompareMethod.Text) = 0
                            chaine = sortie.ReadLine
                            If InStr(chaine, "info", CompareMethod.Text) > 0 _
                            And InStr(chaine, "string", CompareMethod.Text) > 0 _
                            And (InStr(chaine, "collision", CompareMethod.Text) > 0 Or InStr(chaine, "duplicate", CompareMethod.Text) > 0) Then
                                moteurEntete = Replace(chaine, fichierEXP, nomFichier(fichierEXP)) & vbCrLf
                            End If
                            Threading.Thread.Sleep(1)
                        End While

                        If fichierEXP <> "" And Not My.Computer.FileSystem.FileExists(fichierEXP) Then
                            If InStr(moteur_court, "eman 7", CompareMethod.Text) > 0 Or InStr(moteur_court, "eman 8", CompareMethod.Text) > 0 _
                            Or InStr(moteur_court, "hypnos", CompareMethod.Text) > 0 Or InStr(moteur_court, "stockfishmz", CompareMethod.Text) > 0 Then
                                My.Computer.FileSystem.WriteAllText(fichierEXP, "SugaR Experience version 2", False, New System.Text.UTF8Encoding(False))
                            End If
                            entree.WriteLine("setoption name Experience File value " & fichierEXP)
                        End If
                    End If
                End If
            Next
        End If

        entree.WriteLine("isready")
        chaine = ""
        While InStr(chaine, "readyok") = 0
            chaine = sortie.ReadLine
            Threading.Thread.Sleep(1)
        End While
    End Sub

    Public Function coupsEN(coups As String) As String
        Dim chaine As String

        chaine = Replace(coups, "F", "B")
        chaine = Replace(chaine, "C", "N")
        chaine = Replace(chaine, "D", "Q")
        chaine = Replace(chaine, "R", "K")
        chaine = Replace(chaine, "T", "R")

        Return chaine
    End Function

    Public Function coupsFR(coups As String) As String
        Dim chaine As String

        chaine = Replace(coups, "B", "F")
        chaine = Replace(chaine, "N", "C")
        chaine = Replace(chaine, "Q", "D")
        chaine = Replace(chaine, "R", "T")
        chaine = Replace(chaine, "K", "R")

        Return chaine
    End Function

    Public Function cpu(Optional reel As Boolean = False) As Integer
        Dim collection As New ManagementObjectSearcher("select * from Win32_Processor"), taches As Integer
        taches = 0

        For Each element As ManagementObject In collection.Get
            If reel Then
                taches = taches + element.Properties("NumberOfCores").Value 'cores
            Else
                taches = taches + element.Properties("NumberOfLogicalProcessors").Value 'threads
            End If
        Next

        Return taches
    End Function

    Public Sub dechargerMoteur()
        entree.Close()
        sortie.Close()
        processus.Close()

        entree = Nothing
        sortie = Nothing
        processus = Nothing
    End Sub

    Public Function defragBIN(cheminBIN As String, profMin As Integer) As String
        Dim tabBIN(0) As Byte, i As Long, tabNEW() As Byte, offset As Long
        Dim tabTampon(23) As Byte, compteur As Integer, nbSuppression As Integer
        Dim message As String, pos As Long
        Dim lectureBIN As IO.FileStream, posLecture As Long, tailleBIN As Long, tailleTampon As Long, reservation As Boolean

        message = ""

        If My.Computer.FileSystem.FileExists(cheminBIN & ".bak") Then
            My.Computer.FileSystem.DeleteFile(cheminBIN & ".bak")
        End If

        posLecture = 0
        tailleBIN = FileLen(cheminBIN)
        tailleTampon = tailleBIN
        i = 50
        lectureBIN = New IO.FileStream(cheminBIN, IO.FileMode.Open, IO.FileAccess.Read, IO.FileShare.ReadWrite)

        compteur = 0
        nbSuppression = 0

        While posLecture < tailleBIN
            If posLecture + tailleTampon <= tailleBIN Then
                reservation = False
                Do
                    Try
                        ReDim tabBIN(tailleTampon - 1)
                        reservation = True
                    Catch ex As Exception
                        i = i - 1
                        tailleTampon = 24 * i * 1000000
                    End Try
                Loop Until reservation
                lectureBIN.Read(tabBIN, 0, tabBIN.Length)
            Else
                tailleTampon = tailleBIN - posLecture
                ReDim tabBIN(tailleTampon - 1)
                lectureBIN.Read(tabBIN, 0, tabBIN.Length)
            End If

            ReDim tabNEW(UBound(tabBIN))
            pos = 0
            offset = 0
            Do
                Array.Copy(tabBIN, pos, tabTampon, 0, 24)
                If profMin <= tabTampon(8) Then
                    Array.Copy(tabTampon, 0, tabNEW, offset * 24, 24)
                    offset = offset + 1
                Else
                    nbSuppression = nbSuppression + 1
                End If

                compteur = compteur + 1
                pos = pos + 24
            Loop While pos < tabBIN.Length

            tabBIN = Nothing

            ReDim Preserve tabNEW(offset * 24 - 1)
            My.Computer.FileSystem.WriteAllBytes(cheminBIN & ".bak", tabNEW, True)

            tabNEW = Nothing

            posLecture = lectureBIN.Position
        End While

        lectureBIN.Close()

        message = "info string " & nomFichier(cheminBIN) & " -> Total moves: " & compteur & ". Empty moves: " & nbSuppression & ". Fragmentation: " & Format(nbSuppression / compteur, "0.00%") & vbCrLf
        message = message & "info string Saved " & Format(compteur - nbSuppression, "0 moves") & " to " & nomFichier(cheminBIN) & " file"

        If compteur > nbSuppression Then
            My.Computer.FileSystem.DeleteFile(cheminBIN)
            My.Computer.FileSystem.RenameFile(cheminBIN & ".bak", nomFichier(cheminBIN))
        End If

        Return message
    End Function

    Public Function defragEXP(cheminEXP As String, profMin As Integer, verbose As Boolean, entreeDefrag As System.IO.StreamWriter, sortieDefrag As System.IO.StreamReader) As String
        Dim chaine As String, message As String

        message = ""

        entreeDefrag.WriteLine("exp-defrag")
        entreeDefrag.WriteLine(cheminEXP)
        entreeDefrag.WriteLine(profMin)

        entreeDefrag.WriteLine("isready")
        chaine = ""
        While InStr(chaine, "readyok") = 0
            chaine = sortieDefrag.ReadLine
            If verbose Then
                If InStr(chaine, "Defragging", CompareMethod.Text) > 0 Then
                    Console.WriteLine(Replace(Replace(chaine, ": ", " : "), "). ", ")" & vbCrLf))
                ElseIf InStr(chaine, "Writing") > 0 Then
                    Console.WriteLine(Replace(Replace(Replace(chaine, My.Application.Info.DirectoryPath & "\", ""), "[", ""), "]", ""))
                End If
            End If
            Threading.Thread.Sleep(1)
        End While

        If My.Computer.FileSystem.FileExists(cheminEXP & ".bak") Then
            My.Computer.FileSystem.DeleteFile(cheminEXP & ".bak")
        End If

        If My.Computer.FileSystem.FileExists(cheminEXP) Then

            entreeDefrag.WriteLine("setoption name Experience File value <empty>")
            entreeDefrag.WriteLine("isready")
            chaine = ""
            While InStr(chaine, "readyok") = 0
                chaine = sortieDefrag.ReadLine
                Threading.Thread.Sleep(1)
            End While

            entreeDefrag.WriteLine("setoption name Experience File value " & cheminEXP)
            message = ""
            While message = ""
                chaine = sortieDefrag.ReadLine
                If InStr(chaine, "info", CompareMethod.Text) > 0 _
                And InStr(chaine, "string", CompareMethod.Text) > 0 _
                And (InStr(chaine, "collision", CompareMethod.Text) > 0 Or InStr(chaine, "duplicate", CompareMethod.Text) > 0) Then
                    message = Replace(chaine, cheminEXP, nomFichier(cheminEXP)) & vbCrLf
                End If
                Threading.Thread.Sleep(1)
            End While

            entreeDefrag.WriteLine("isready")
            chaine = ""
            While InStr(chaine, "readyok") = 0
                chaine = sortieDefrag.ReadLine
                Threading.Thread.Sleep(1)
            End While
        End If

        Return message
    End Function

    Public Function defragHypnos(cheminEXE As String, cheminEXP As String, entreeDefrag As System.IO.StreamWriter, sortieDefrag As System.IO.StreamReader) As String
        Dim chaine As String, message As String
        Dim commande As New Process()

        message = ""

        'defrag
        commande.StartInfo.FileName = cheminEXE
        commande.StartInfo.WorkingDirectory = My.Computer.FileSystem.GetParentPath(cheminEXE)
        commande.StartInfo.Arguments = " defrag " & nomFichier(cheminEXP)
        commande.StartInfo.UseShellExecute = False
        commande.Start()
        commande.WaitForExit()

        'nettoyage
        If My.Computer.FileSystem.FileExists(cheminEXP & ".bak") Then
            My.Computer.FileSystem.DeleteFile(cheminEXP & ".bak")
        End If

        'maj
        If My.Computer.FileSystem.FileExists(cheminEXP) Then

            entreeDefrag.WriteLine("setoption name Experience File value <empty>")
            entreeDefrag.WriteLine("isready")
            chaine = ""
            While InStr(chaine, "readyok") = 0
                chaine = sortieDefrag.ReadLine
                Threading.Thread.Sleep(1)
            End While

            entreeDefrag.WriteLine("setoption name Experience File value " & cheminEXP)
            message = ""
            While message = ""
                chaine = sortieDefrag.ReadLine
                If InStr(chaine, "info", CompareMethod.Text) > 0 _
                And InStr(chaine, "string", CompareMethod.Text) > 0 _
                And (InStr(chaine, "collision", CompareMethod.Text) > 0 Or InStr(chaine, "duplicate", CompareMethod.Text) > 0) Then
                    message = Replace(chaine, cheminEXP, nomFichier(cheminEXP)) & vbCrLf
                End If
                Threading.Thread.Sleep(1)
            End While

            entreeDefrag.WriteLine("isready")
            chaine = ""
            While InStr(chaine, "readyok") = 0
                chaine = sortieDefrag.ReadLine
                Threading.Thread.Sleep(1)
            End While
        End If

        Return message
    End Function

    Public Function droite(texte As String, longueur As Integer) As String
        If longueur > 0 Then
            Return VB.Right(texte, longueur)
        Else
            Return ""
        End If
    End Function

    Public Function emanQuality(suiteUCI As String, suiteFR As String, Optional startpos As String = "") As String
        Dim chaine As String, tabChaine() As String, score As Single, ligne As String, mat As String, epd As String

        ligne = ""
        If InStr(suiteUCI, " ") = 0 Then
            If startpos = "" Then
                entree.WriteLine("position startpos")
            Else
                entree.WriteLine("position fen " & startpos)
            End If
        Else
            If startpos = "" Then
                entree.WriteLine("position startpos moves " & suiteUCI.Substring(0, suiteUCI.LastIndexOf(" ")))
            Else
                entree.WriteLine("position fen " & startpos & " moves " & suiteUCI.Substring(0, suiteUCI.LastIndexOf(" ")))
            End If
        End If
        entree.WriteLine("expex")

        entree.WriteLine("isready")

        chaine = ""
        epd = ""
        While InStr(chaine, "readyok") = 0
            chaine = sortie.ReadLine
            If InStr(chaine, "Fen: ", CompareMethod.Text) > 0 Then
                epd = Replace(chaine, "Fen: ", "")
            ElseIf ligne = "" And gauche(chaine, 4) = "1 : " And InStr(chaine, "quality:") > 0 Then
                ligne = chaine
            End If
            Threading.Thread.Sleep(1)
        End While
        chaine = ligne

        If InStr(chaine, "quality:") = 0 Then
            chaine = ""
        Else
            chaine = Replace(chaine, "  ", "")
            chaine = Replace(chaine, ",", ":")
            chaine = Replace(chaine, ": ", ":")
            chaine = Replace(chaine, " :", ":")

            tabChaine = Split(chaine, ":")

            mat = "mate "
            If InStr(moteur_court, "eman 6", CompareMethod.Text) > 0 Then
                If InStr(tabChaine(7), mat) = 0 Then
                    score = CSng(CInt(Trim(Replace(tabChaine(7), "cp ", ""))) / 100)
                    mat = ""
                Else
                    score = CInt(Trim(Replace(tabChaine(7), mat, "")))
                End If

                If InStr(epd, " b ", CompareMethod.Text) > 0 Then
                    score = -score
                End If

                tabChaine(1) = analyseCoups(tabChaine(1), suiteFR, startpos)

                chaine = StrDup(7 - Len(tabChaine(1)), " ") & tabChaine(1)

                If mat <> "" Then
                    chaine = chaine & " {" & mat & Format(score, "0") & "/" & tabChaine(5) & "}"
                    chaine = Replace(chaine, "{mate -", "{-M")
                    chaine = Replace(chaine, "{mate ", "{+M")
                Else
                    If score > 0 Then
                        chaine = chaine & " {+" & Format(score, "0.00") & "/" & tabChaine(5) & "}"
                    ElseIf score = 0 Then
                        chaine = chaine & " { 0.00/" & tabChaine(5) & "}"
                    Else
                        chaine = chaine & " {" & Format(score, "0.00") & "/" & tabChaine(5) & "}" 'score contient le "-"
                    End If
                End If
            ElseIf InStr(moteur_court, "eman 7", CompareMethod.Text) > 0 Or InStr(moteur_court, "eman 8", CompareMethod.Text) > 0 _
                Or InStr(moteur_court, "hypnos", CompareMethod.Text) > 0 Or InStr(moteur_court, "stockfishmz", CompareMethod.Text) > 0 Then
                If InStr(tabChaine(5), mat) = 0 Then
                    score = CSng(CInt(Trim(Replace(tabChaine(5), "cp ", ""))) / 100)
                    mat = ""
                Else
                    score = CInt(Trim(Replace(tabChaine(5), mat, "")))
                End If

                If InStr(epd, " b ", CompareMethod.Text) > 0 Then
                    score = -score
                End If

                tabChaine(1) = analyseCoups(tabChaine(1), suiteFR, startpos)

                chaine = StrDup(7 - Len(tabChaine(1)), " ") & tabChaine(1)

                If mat <> "" Then
                    chaine = chaine & " {" & mat & Format(score, "0") & "/" & tabChaine(3) & "}"
                    chaine = Replace(chaine, "{mate -", "{-M")
                    chaine = Replace(chaine, "{mate ", "{+M")
                Else
                    If score > 0 Then
                        chaine = chaine & " {+" & Format(score, "0.00") & "/" & tabChaine(3) & "}"
                    ElseIf score = 0 Then
                        chaine = chaine & " { 0.00/" & tabChaine(3) & "}"
                    Else
                        chaine = chaine & " {" & Format(score, "0.00") & "/" & tabChaine(3) & "}" 'score contient le "-"
                    End If
                End If
            End If

        End If

        Return Replace(chaine, ",", ".")
    End Function

    Public Sub entreeEXP(ByRef tabEXP() As Byte, positionEPD As String, coupUCI As String, scoreCP As Integer, facteur As Integer, prof As Integer, compteur As Integer, entreeDefrag As System.IO.StreamWriter, sortieDefrag As System.IO.StreamReader, ByRef moteur_court As String)
        Dim compteurHEX As String

        'rnbqkbnr/2pppppp/1p5B/p7/3P4/P7/1PP1PPPP/RN1QKBNR b KQkq - 1 3
        '6078880BD90221F8
        'F8(248) 21(33) 02(2) D9(217) 0B(11) 88(136) 78(120) 60(96)
        ' 0       1      2     3       4      5       6       7
        Array.Copy(epdToEXP(entreeDefrag, sortieDefrag, positionEPD), 0, tabEXP, 0, 8)

        'g8h6
        '0 000 111(8) 110(g) 101(6) 111(h)
        '0000(0) 1111(F) 1010(A) 1111(F)
        'AF(175) OF(15)
        ' 8       9
        Array.Copy(moveToEXP(coupUCI), 0, tabEXP, 8, 2)

        If InStr(moteur_court, "eman 6", CompareMethod.Text) > 0 Then
            'score cp 936
            '450
            scoreCP = CInt(CInt(scoreCP) * 100 / facteur)
            'hex 0384
            '84(132) 03(3)
            ' a       b
            Array.Copy(scoreToEXP(scoreCP), 0, tabEXP, 10, 2)

            'depth 14
            '0E(14)
            ' c
            tabEXP(12) = prof

            tabEXP(13) = 0 'd
            tabEXP(14) = 0 'e
            tabEXP(15) = 0 'f

            'count 3109
            'hex 0C25
            '25(37) 0C(12)
            ' 0      1
            compteurHEX = Hex(compteur)
            compteurHEX = StrDup(4 - Len(compteurHEX), "0") & compteurHEX
            Array.Copy(inverseurHEX(compteurHEX, 2), 0, tabEXP, 16, 2)

            tabEXP(18) = 0 '2
            tabEXP(19) = 0 '3

            '??? N/A
            tabEXP(20) = 0 '4
            tabEXP(21) = 128 '5

            'next eval N/A
            tabEXP(22) = 0 '6
            tabEXP(23) = 128 '7

            'next x eval N/A
            tabEXP(24) = 0 '8
            tabEXP(25) = 128 '9

            '??? 0
            tabEXP(26) = 0 'a
            tabEXP(27) = 0 'b

            'padding
            tabEXP(28) = 0 'c
            tabEXP(29) = 0 'd
            tabEXP(30) = 0 'e
            tabEXP(31) = 0 'f

        ElseIf InStr(moteur_court, "eman 7", CompareMethod.Text) > 0 Or InStr(moteur_court, "eman 8", CompareMethod.Text) > 0 _
            Or InStr(moteur_court, "hypnos", CompareMethod.Text) > 0 Or InStr(moteur_court, "stockfishmz", CompareMethod.Text) > 0 Or InStr(moteur_court, "aurora", CompareMethod.Text) > 0 Then

            tabEXP(10) = 0 'a
            tabEXP(11) = 0 'b

            'score cp 936
            '450
            scoreCP = CInt(CInt(scoreCP) * 100 / facteur)
            'hex 0384
            '84(132) 03(3) 00(0) 00(0)
            ' c       d     e     f
            Array.Copy(scoreToEXP(scoreCP), 0, tabEXP, 12, 2)
            If scoreCP > 0 Then
                tabEXP(14) = 0 'e
                tabEXP(15) = 0 'f
            Else
                tabEXP(14) = 255 'e
                tabEXP(15) = 255 'f
            End If

            'depth 14
            '0E(14)
            ' 0
            tabEXP(16) = prof

            tabEXP(17) = 0 '1
            tabEXP(18) = 0 '2
            tabEXP(19) = 0 '3

            'count 3109
            'hex 0C25
            '25(37) 0C(12)
            ' 4      5    
            compteurHEX = Hex(compteur)
            compteurHEX = StrDup(4 - Len(compteurHEX), "0") & compteurHEX
            Array.Copy(inverseurHEX(compteurHEX, 2), 0, tabEXP, 20, 2)

            tabEXP(22) = 0 '6
            tabEXP(23) = 0 '7

        End If
    End Sub

    Public Function epdCasesOccupees(fen As String) As String
        Dim tabColonne() As String, casesOccupees As String, tabLignes() As String, chaine As String, caractere As String
        Dim ligne As Integer, index As Integer, coups As String

        tabColonne = {"", "a", "b", "c", "d", "e", "f", "g", "h"}
        casesOccupees = "a1;b1;c1;d1;e1;f1;g1;h1;a2;b2;c2;d2;e2;f2;g2;h2;a7;b7;c7;d7;e7;f7;g7;h7;a8;b8;c8;d8;e8;f8;g8;h8;"
        coups = ""

        'pour chaque ligne
        tabLignes = Split(gauche(fen, fen.IndexOf(" ")), "/")
        For ligne = UBound(tabLignes) To 0 Step -1
            'on remplace les chiffres par des "-" qui indique "case vide"
            chaine = ""
            For index = 0 To Len(tabLignes(ligne)) - 1
                caractere = tabLignes(ligne).Substring(index, 1)
                If IsNumeric(caractere) Then
                    chaine = chaine & StrDup(CInt(caractere), "-")
                Else
                    chaine = chaine & caractere
                End If
                Threading.Thread.Sleep(1)
            Next
            tabLignes(ligne) = chaine

            'on met à jour les cases occupées
            For index = 0 To Len(tabLignes(ligne)) - 1
                If tabLignes(ligne).Substring(index, 1) = "-" Then
                    'on efface
                    casesOccupees = Replace(casesOccupees, tabColonne(index + 1) & Format(tabLignes.Length - ligne) & ";", "")
                Else
                    'on ajoute
                    chaine = tabColonne(index + 1) & Format(tabLignes.Length - ligne)
                    caractere = tabLignes(ligne).Substring(index, 1)
                    If caractere = "p" Or caractere = "P" Then
                        caractere = ""
                    End If
                    coups = coups & UCase(caractere) & chaine & "-" & chaine & " "
                    If InStr(casesOccupees, chaine & ";") = 0 Then
                        casesOccupees = casesOccupees & chaine & ";"
                    End If
                End If
                Threading.Thread.Sleep(1)
            Next
            Threading.Thread.Sleep(1)
        Next

        casesOccupees = trierChaine(casesOccupees, ";") & ";"

        Return casesOccupees & ":" & coupsFR(coups)
    End Function

    Public Function epdToEXP(entreeDefrag As System.IO.StreamWriter, sortieDefrag As System.IO.StreamReader, Optional startpos As String = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") As Byte()
        Dim key As String

        entreedefrag.WriteLine("position fen " & startpos)

        entreedefrag.WriteLine("d")

        key = ""
        While InStr(key, "Key: ", CompareMethod.Text) = 0
            key = sortiedefrag.ReadLine
        End While

        key = Replace(key, "Key: ", "")

        '6078880BD90221F8 =>  F8 21 02  D9 0B  88  78 60
        '                    248 33  2 217 11 136 120 96

        Return inverseurHEX(key, 8)

    End Function

    Public Function expListe(positionUCI As String) As String
        Dim chaine As String, liste As String

        entree.WriteLine(positionUCI)

        entree.WriteLine("isready")
        chaine = ""
        While InStr(chaine, "readyok") = 0
            chaine = sortie.ReadLine
            Threading.Thread.Sleep(1)
        End While

        entree.WriteLine("expex")

        entree.WriteLine("isready")
        chaine = ""
        liste = ""
        While InStr(chaine, "readyok") = 0
            chaine = sortie.ReadLine
            If InStr(chaine, "quality:") > 0 Then
                liste = liste & chaine & vbCrLf
            End If
        End While

        Return liste
    End Function

    Public Function formatage(info As String, index As Integer) As String
        Dim tabChaine() As String, i As Integer, chaine As String
        Dim prof As String, score As Single, temps As String, mat As String

        'de ça : info depth 14 score cp 7148 time 30 pv g5g7
        'vers ça : g5g7 {7148/14 30}

        prof = ""
        score = -1000000
        temps = ""
        mat = ""
        tabChaine = Split(info, " ")
        For i = 0 To UBound(tabChaine)
            If tabChaine(i) = "depth" Then

                prof = tabChaine(i + 1)

            ElseIf tabChaine(i) = "score" And tabChaine(i + 1) = "cp" Then '7148 cp

                'bestmove = on simule le coup, on analyse le meilleur coup adverse, on inversera le score
                If mode = "bestmove" Then 'If InStr(moteur_court, "eman", CompareMethod.Text) > 0 Then
                    score = -CSng(CInt(tabChaine(i + 2)) / 100) '71.48
                ElseIf mode = "searchmoves" Then 'searchmoves : on limite la recherche au coup à analyser
                    score = CSng(CInt(tabChaine(i + 2)) / 100) '71.48
                End If

                'on inverse le score des noirs uniquement
                If index Mod 2 = 0 Then
                    score = -score
                End If

            ElseIf tabChaine(i) = "score" And tabChaine(i + 1) = "mate" Then

                mat = "mate "
                'bestmove = on simule le coup, on analyse le meilleur coup adverse, on inversera le score
                If mode = "bestmove" Then 'If InStr(moteur_court, "eman", CompareMethod.Text) > 0 Then
                    score = -CInt(tabChaine(i + 2))
                ElseIf mode = "searchmoves" Then 'searchmoves : on limite la recherche au coup à analyser
                    score = CInt(tabChaine(i + 2))
                End If

                'on inverse le score des noirs uniquement
                If index Mod 2 = 0 Then
                    score = -score
                End If

            ElseIf tabChaine(i) = "time" Then

                temps = CInt(CInt(tabChaine(i + 1)) / 1000)

            End If
            If score <> -1000000 And prof <> "" And temps <> "" Then
                Exit For
            End If
        Next

        chaine = ""
        If mat <> "" Then
            chaine = "{" & mat & Format(score, "0") & "/" & prof & " " & temps & "}"
            chaine = Replace(chaine, "{mate -", "{-M")
            chaine = Replace(chaine, "{mate ", "{+M")
        Else
            If score > 0 Then
                chaine = "{+" & Format(score, "0.00") & "/" & prof & " " & temps & "}"
            ElseIf score = 0 Then
                chaine = "{ 0.00/" & prof & " " & temps & "}"
            Else
                chaine = "{" & Format(score, "0.00") & "/" & prof & " " & temps & "}" 'score contient le "-"
            End If
        End If

        Return Replace(chaine, ",", ".")
    End Function

    Public Function formaterCoups(mode As String, coups As String, Optional index As Integer = 0) As String
        Dim chaine As String, i As Integer, tabChaine() As String

        chaine = Replace(coups, "+", "")
        chaine = Replace(chaine, " mate", "")

        Select Case mode
            Case "arena", "texte"
                'indexation
                If index > 0 Then
                    chaine = Format(Int(index / 2) + 1) & ". " & chaine
                End If

            Case "pgn"
                'on supprime les commentaires
                chaine = Replace(Replace(chaine, "??", ""), "!!", "")
                'indexation
                If index > 0 Then
                    chaine = Format(Int(index / 2) + 1) & ". " & chaine
                End If

            Case "moteur"
                'on intercepte les roques
                'coups blanc
                If (index Mod 2) = 1 Then
                    If InStr(chaine, "0-0-0", CompareMethod.Text) > 0 Then
                        chaine = "e1c1"
                    ElseIf InStr(chaine, "0-0", CompareMethod.Text) > 0 Then
                        chaine = "e1g1"
                    End If
                Else
                    'coups noir
                    If InStr(chaine, "0-0-0", CompareMethod.Text) > 0 Then
                        chaine = "e8c8"
                    ElseIf InStr(chaine, "0-0", CompareMethod.Text) > 0 Then
                        chaine = "e8g8"
                    End If
                End If
                'on supprime les commentaires
                chaine = Replace(Replace(chaine, "??", ""), "!!", "")
                'on retient uniquement la postion de départ/arrivée
                chaine = Replace(Replace(chaine, "-", ""), "x", "")
                'on supprime la prise en passant
                chaine = Replace(chaine, "ep", "")
                'on supprime l'info du type de pièce
                If Len(chaine) = 5 And IsNumeric(droite(chaine, 1)) Then
                    chaine = droite(chaine, 4)
                ElseIf InStr(chaine, "=", CompareMethod.Text) > 0 Then
                    chaine = coupsEN(Replace(chaine, "=", "")) 'g7g8=C
                End If

            Case "doublon"
                'on supprime les commentaires
                chaine = Replace(Replace(chaine, "??", ""), "!!", "")
                'on supprime l'indexation
                tabChaine = Split(chaine, " ")
                chaine = ""
                For i = 0 To UBound(tabChaine)
                    If InStr(tabChaine(i), ".", CompareMethod.Text) = 0 Then
                        chaine = chaine & tabChaine(i) & " "
                    End If
                Next
                chaine = Trim(chaine)

            Case Else
                'on supprime que les commentaires
                chaine = Replace(Replace(chaine, "??", ""), "!!", "")

        End Select

        Return chaine
    End Function

    Public Function gauche(texte As String, longueur As Integer) As String
        If longueur > 0 Then
            Return VB.Left(texte, longueur)
        Else
            Return ""
        End If
    End Function

    Public Function heureFin(depart As Integer, i As Long, max As Long, Optional reprise As Long = 0, Optional formatCourt As Boolean = False) As String
        If formatCourt Then
            Return Format(DateAdd(DateInterval.Second, (max - i) * ((Environment.TickCount - depart) / 1000) / (i - reprise), Now), "dd/MM/yy HH:mm:ss")
        Else
            Return Format(DateAdd(DateInterval.Second, (max - i) * ((Environment.TickCount - depart) / 1000) / (i - reprise), Now), "dddd' 'd' 'MMM' @ 'HH'h'mm'm'ss")
        End If
    End Function

    Public Function hexa(valeur As Integer) As String
        Dim chaine As String

        chaine = Hex(valeur)
        If Len(chaine) = 1 Then
            chaine = "0" & chaine
        End If
        Return chaine

    End Function

    Public Function hexadecimalToBinaire(hexadecimal As String) As String
        Dim entier As Integer, chaine As String

        entier = Convert.ToInt64(hexadecimal, 16)
        chaine = ""

        Do
            chaine = Format(entier Mod 2) & chaine
            entier = Fix(entier / 2)
        Loop While (entier / 2 > 0)
        chaine = CDbl(chaine).ToString(StrDup(4 * Len(hexadecimal), "0"))
        Return chaine
    End Function

    Public Function inverseurHEX(chaine As String, taille As Integer) As Byte()
        Dim i As Integer, index As Integer, tab(taille - 1) As Byte

        index = 0
        For i = Len(chaine) To 2 Step -2
            tab(index) = Convert.ToInt64(droite(gauche(chaine, i), 2), 16)
            index = index + 1
        Next

        Return tab
    End Function

    Public Function moteurEPD(moteur As String, moves As String, Optional startpos As String = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") As String
        Dim processusEPD As New System.Diagnostics.Process(), entreeEPD As System.IO.StreamWriter, sortieEPD As System.IO.StreamReader, chaineEPD As String

        chaineEPD = startpos
        If moves <> "" Then
            'on charge le moteur
            processusEPD.StartInfo.RedirectStandardOutput = True
            processusEPD.StartInfo.UseShellExecute = False
            processusEPD.StartInfo.RedirectStandardInput = True
            processusEPD.StartInfo.RedirectStandardError = True
            processusEPD.StartInfo.CreateNoWindow = True
            processusEPD.StartInfo.FileName = moteur
            processusEPD.Start()

            entreeEPD = processusEPD.StandardInput
            sortieEPD = processusEPD.StandardOutput

            entreeEPD.WriteLine("position fen " & startpos & " moves " & moves)

            entreeEPD.WriteLine("d")

            chaineEPD = ""
            While InStr(chaineEPD, "Fen: ", CompareMethod.Text) = 0
                chaineEPD = sortieEPD.ReadLine
            End While
            entreeEPD.WriteLine("quit")

            entreeEPD.Close()
            sortieEPD.Close()
            processusEPD.Close()

        End If

        entreeEPD = Nothing
        sortieEPD = Nothing
        processusEPD = Nothing

        Return Replace(chaineEPD, "Fen: ", "")
    End Function

    Public Function moveToEXP(coup As String, Optional litteral As Boolean = False) As Byte()
        Dim coupBIN As String, i As Integer, cumul As Integer, coupHEX As String

        'g8h6
        '0 000 111(8) 110(g) 101(6) 111(h)

        coupBIN = ""

        If Not litteral Then
            If coup = "e1c1" Then
                coupBIN = "1 100 000 100 000 000" 'equivalent e1a1

            ElseIf coup = "e8c8" Then
                coupBIN = "1 100 111 100 111 000" 'equivalent e8a8

            ElseIf coup = "e1g1" Then
                coupBIN = "1 100 000 100 000 111" 'equivalent e1h1

            ElseIf coup = "e8g8" Then
                coupBIN = "1 100 111 100 111 111" 'equivalent e8h8
            End If
        End If

        If coupBIN = "" Then
            coupBIN = "0 000 "
            If Len(coup) = 5 Then
                Select Case droite(coup, 1)
                    Case "N", "n"
                        coupBIN = "0 001 "
                    Case "B", "b"
                        coupBIN = "0 101 "
                    Case "R", "r"
                        coupBIN = "0 110 "
                    Case "Q", "q"
                        coupBIN = "0 111 "
                End Select
            End If

            'ligne de départ
            If coup.Substring(1, 1) = "1" Then
                coupBIN = coupBIN & "000" & " "
            ElseIf coup.Substring(1, 1) = "2" Then
                coupBIN = coupBIN & "001" & " "
            ElseIf coup.Substring(1, 1) = "3" Then
                coupBIN = coupBIN & "010" & " "
            ElseIf coup.Substring(1, 1) = "4" Then
                coupBIN = coupBIN & "011" & " "
            ElseIf coup.Substring(1, 1) = "5" Then
                coupBIN = coupBIN & "100" & " "
            ElseIf coup.Substring(1, 1) = "6" Then
                coupBIN = coupBIN & "101" & " "
            ElseIf coup.Substring(1, 1) = "7" Then
                coupBIN = coupBIN & "110" & " "
            ElseIf coup.Substring(1, 1) = "8" Then
                coupBIN = coupBIN & "111" & " "
            End If

            'colonne de départ
            If coup.Substring(0, 1) = "a" Then
                coupBIN = coupBIN & "000" & " "
            ElseIf coup.Substring(0, 1) = "b" Then
                coupBIN = coupBIN & "001" & " "
            ElseIf coup.Substring(0, 1) = "c" Then
                coupBIN = coupBIN & "010" & " "
            ElseIf coup.Substring(0, 1) = "d" Then
                coupBIN = coupBIN & "011" & " "
            ElseIf coup.Substring(0, 1) = "e" Then
                coupBIN = coupBIN & "100" & " "
            ElseIf coup.Substring(0, 1) = "f" Then
                coupBIN = coupBIN & "101" & " "
            ElseIf coup.Substring(0, 1) = "g" Then
                coupBIN = coupBIN & "110" & " "
            ElseIf coup.Substring(0, 1) = "h" Then
                coupBIN = coupBIN & "111" & " "
            End If

            'ligne d'arrivée
            If coup.Substring(3, 1) = "1" Then
                coupBIN = coupBIN & "000" & " "
            ElseIf coup.Substring(3, 1) = "2" Then
                coupBIN = coupBIN & "001" & " "
            ElseIf coup.Substring(3, 1) = "3" Then
                coupBIN = coupBIN & "010" & " "
            ElseIf coup.Substring(3, 1) = "4" Then
                coupBIN = coupBIN & "011" & " "
            ElseIf coup.Substring(3, 1) = "5" Then
                coupBIN = coupBIN & "100" & " "
            ElseIf coup.Substring(3, 1) = "6" Then
                coupBIN = coupBIN & "101" & " "
            ElseIf coup.Substring(3, 1) = "7" Then
                coupBIN = coupBIN & "110" & " "
            ElseIf coup.Substring(3, 1) = "8" Then
                coupBIN = coupBIN & "111" & " "
            End If

            'colonne d'arrivée
            If coup.Substring(2, 1) = "a" Then
                coupBIN = coupBIN & "000"
            ElseIf coup.Substring(2, 1) = "b" Then
                coupBIN = coupBIN & "001"
            ElseIf coup.Substring(2, 1) = "c" Then
                coupBIN = coupBIN & "010"
            ElseIf coup.Substring(2, 1) = "d" Then
                coupBIN = coupBIN & "011"
            ElseIf coup.Substring(2, 1) = "e" Then
                coupBIN = coupBIN & "100"
            ElseIf coup.Substring(2, 1) = "f" Then
                coupBIN = coupBIN & "101"
            ElseIf coup.Substring(2, 1) = "g" Then
                coupBIN = coupBIN & "110"
            ElseIf coup.Substring(2, 1) = "h" Then
                coupBIN = coupBIN & "111"
            End If
        End If

        '0 000 111(8) 110(g) 101(6) 111(h)
        coupBIN = Replace(coupBIN, " ", "")

        '0000111110101111
        cumul = 0
        For i = 1 To Len(coupBIN)
            cumul = cumul + CInt(gauche(droite(coupBIN, i), 1)) * 2 ^ (i - 1)
        Next
        coupHEX = Hex(cumul)
        coupHEX = StrDup(CInt(Len(coupBIN) / 4 - Len(coupHEX)), "0") & coupHEX

        '0000(0) 1111(F) 1010(A) 1111(F)
        'AF(175) OF(15)

        Return inverseurHEX(coupHEX, 2)
    End Function

    Public Function nbCaracteres(ByVal chaine As String, ByVal critere As String) As Integer
        Return Len(chaine) - Len(Replace(chaine, critere, ""))
    End Function

    Public Function nbChaines(ByVal chaine As String, ByVal critere As String) As Integer
        Dim compteur As Integer, pos As Integer

        compteur = 0
        pos = 0
        Do
            pos = chaine.IndexOf(critere, pos)
            If pos >= 0 Then
                compteur = compteur + 1
                pos = pos + Len(critere)
            End If
        Loop Until pos = -1

        Return compteur
    End Function

    Public Function nomFichier(chemin As String) As String
        Return My.Computer.FileSystem.GetName(chemin)
    End Function

    Public Sub nouvellePartie()
        Dim chaine As String

        entree.WriteLine("stop")
        entree.WriteLine("isready")
        chaine = ""
        While InStr(chaine, "readyok") = 0
            chaine = sortie.ReadLine
            Threading.Thread.Sleep(1)
        End While

        entree.WriteLine("ucinewgame")
        entree.WriteLine("isready")
        chaine = ""
        While InStr(chaine, "readyok") = 0
            chaine = sortie.ReadLine
            Threading.Thread.Sleep(1)
        End While

    End Sub

    Public Sub pgnUCI(chemin As String, fichier As String, suffixe As String, Optional priorite As Integer = 64)
        Dim nom As String, commande As New Process()
        Dim dossierFichier As String, dossierTravail As String

        nom = Replace(nomFichier(fichier), ".pgn", "")

        dossierFichier = fichier.Substring(0, fichier.LastIndexOf("\"))
        dossierTravail = My.Computer.FileSystem.GetParentPath(chemin)

        'si pgn-extract.exe ne se trouve à l'emplacement prévu (par <nom_ordinateur>.ini)
        If Not My.Computer.FileSystem.FileExists(dossierTravail & "\pgn-extract.exe") Then

            'si pgn-extract.exe ne se trouve dans le même dossier que le notre application
            dossierTravail = Environment.CurrentDirectory
            If Not My.Computer.FileSystem.FileExists(dossierTravail & "\pgn-extract.exe") Then

                'on cherche s'il se trouve dans le même dossier que le fichierPGN
                dossierTravail = dossierFichier
                If Not My.Computer.FileSystem.FileExists(dossierTravail & "\pgn-extract.exe") Then

                    'pgn-extract.exe est introuvable
                    MsgBox("Veuillez copier pgn-extract.exe dans :" & vbCrLf & dossierTravail, MsgBoxStyle.Critical)
                    dossierTravail = Environment.CurrentDirectory
                    If Not My.Computer.FileSystem.FileExists(dossierTravail & "\pgn-extract.exe") Then
                        End
                    End If
                End If
            End If

        End If

        'si le fichierPGN ne se trouve pas dans le dossier de travail
        If dossierFichier <> dossierTravail Then
            'on recopie temporairement le fichierPGN dans le dossierTravail
            My.Computer.FileSystem.CopyFile(fichier, dossierTravail & "\" & nom & ".pgn", True)
        End If

        commande.StartInfo.FileName = dossierTravail & "\pgn-extract.exe"
        commande.StartInfo.WorkingDirectory = dossierTravail

        If InStr(nom, " ") = 0 Then
            commande.StartInfo.Arguments = " -s -Wuci -o" & nom & suffixe & ".pgn" & " " & nom & ".pgn"
        Else
            commande.StartInfo.Arguments = " -s -Wuci -o""" & nom & suffixe & ".pgn""" & " """ & nom & ".pgn"""
        End If

        commande.StartInfo.CreateNoWindow = True
        commande.StartInfo.UseShellExecute = False
        commande.Start()
        commande.PriorityClass = priorite '64 (idle), 16384 (below normal), 32 (normal), 32768 (above normal), 128 (high), 256 (realtime)
        commande.WaitForExit()

        'si le dossierTravail ne correspond pas au dossier du fichierPGN
        If dossierFichier <> dossierTravail Then
            'on déplace le fichier moteur
            Try
                My.Computer.FileSystem.DeleteFile(dossierTravail & "\" & nom & ".pgn")
            Catch ex As Exception

            End Try
            My.Computer.FileSystem.MoveFile(dossierTravail & "\" & nom & suffixe & ".pgn", dossierFichier & "\" & nom & suffixe & ".pgn")
        End If

    End Sub

    Public Function retourExperience(chainePosition As String, coupUCI As String, critereProf As Integer) As String
        Dim chaine As String, tabChaine() As String, tabTmp() As String, chaine_mem As String

        chaine = ""
        chaine_mem = ""
        chaine = expListe(chainePosition)

        If chaine <> "" Then
            tabChaine = Split(chaine, vbCrLf)

            'si le coup recherché figure dans le fichier d'expérience
            For i = 0 To UBound(tabChaine)
                If InStr(tabChaine(i), coupUCI) > 0 Then
                    tabChaine(i) = Replace(tabChaine(i), "  ", "")
                    tabChaine(i) = Replace(tabChaine(i), ",", ":")
                    tabChaine(i) = Replace(tabChaine(i), ": ", ":")
                    tabChaine(i) = Replace(tabChaine(i), " :", ":")
                    tabTmp = Split(tabChaine(i), ":")

                    If InStr(moteur_court, "eman 6", CompareMethod.Text) > 0 Then

                        'experience v1 => 5:f8e7:count:1:depth:43:eval:cp -20:next eval:N/A:next X eval:N/A:quality:-1
                        'tabTmp           0 1    2     3 4     5  6    7      8         9   10          11  12      13
                        'si la profondeur demandé est inférieure ou égale à la profondeur de l'expérience
                        If critereProf <= CInt(tabTmp(5)) Then
                            'on évite de ré-analyser ce coup
                            chaine_mem = "info depth " & tabTmp(5) & " score " & tabTmp(7) & " time " & dureeMoyProf * 1000 & " pv " & coupUCI
                            'info depth 43 score cp -20 time 259000 pv f8e7
                        Else
                            'on devra ré-analyser ce coup pour atteindre une meilleure profondeur (=la profondeur demandée)
                            chaine_mem = ""
                        End If

                    ElseIf InStr(moteur_court, "eman 7", CompareMethod.Text) > 0 Or InStr(moteur_court, "eman 8", CompareMethod.Text) > 0 _
                        Or InStr(moteur_court, "hypnos", CompareMethod.Text) > 0 Or InStr(moteur_court, "stockfishmz", CompareMethod.Text) > 0 Then

                        'experience v2 => 5:f8e7:depth:43:eval:cp -20:count:1:quality:-1"
                        'tabTmp           0 1    2     3  4    5      6     7 8       9
                        'si la profondeur demandé est inférieure ou égale à la profondeur de l'expérience
                        If critereProf <= CInt(tabTmp(3)) Then
                            'on évite de ré-analyser ce coup
                            chaine_mem = "info depth " & tabTmp(3) & " score " & tabTmp(5) & " time " & dureeMoyProf * 1000 & " pv " & coupUCI
                            'info depth 43 score cp -20 time 259000 pv f8e7
                        Else
                            'on devra ré-analyser ce coup pour atteindre une meilleure profondeur (=la profondeur demandée)
                            chaine_mem = ""
                        End If

                    ElseIf InStr(moteur_court, "brainlearn", CompareMethod.Text) > 0 Or InStr(moteur_court, "judas", CompareMethod.Text) > 0 Then

                        'brainlearn    => 5:f8e7:depth:43:eval:cp -20:quality:-1"
                        'tabTmp           0 1    2     3  4    5      6       7
                        'si la profondeur demandé est inférieure ou égale à la profondeur de l'expérience
                        If critereProf <= CInt(tabTmp(3)) Then
                            'on évite de ré-analyser ce coup
                            chaine_mem = "info depth " & tabTmp(3) & " score " & tabTmp(5) & " time " & dureeMoyProf * 1000 & " pv " & coupUCI
                            'info depth 43 score cp -20 time 259000 pv f8e7
                        Else
                            'on devra ré-analyser ce coup pour atteindre une meilleure profondeur (=la profondeur demandée)
                            chaine_mem = ""
                        End If

                    End If

                    Exit For
                End If
            Next
        End If

        Return chaine_mem
    End Function

    Public Function scoreToEXP(eval As Integer) As Byte()
        Dim tab(1) As Byte, scoreHEX As String

        scoreHEX = ""
        If eval > 0 Then
            scoreHEX = Hex(eval * 2)
        Else
            scoreHEX = Hex(eval * 2 + 65535)
        End If
        scoreHEX = StrDup(4 - Len(scoreHEX), "0") & scoreHEX

        Return inverseurHEX(scoreHEX, 2)
    End Function

    Public Function trierChaine(serie As String, separateur As String, Optional ordre As Boolean = True) As String
        Dim tabChaine() As String

        tabChaine = Split(serie, separateur)
        If tabChaine(UBound(tabChaine)) = "" Then
            ReDim Preserve tabChaine(UBound(tabChaine) - 1)
        End If

        Array.Sort(tabChaine)
        If Not ordre Then
            Array.Reverse(tabChaine)
        End If

        Return String.Join(separateur, tabChaine)
    End Function

    Public Function uciKEY(entreeKEY As System.IO.StreamWriter, sortieKEY As System.IO.StreamReader, movesUCI As String, Optional startpos As String = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") As String
        Dim key As String

        If movesUCI <> "" Then
            entreeKEY.WriteLine("position fen " & startpos & " moves " & movesUCI)
        Else
            entreeKEY.WriteLine("position fen " & startpos)
        End If

        entreeKEY.WriteLine("d")

        key = ""
        While InStr(key, "Key: ", CompareMethod.Text) = 0
            key = sortieKEY.ReadLine
        End While

        Return Replace(key, "Key: ", "")
    End Function

End Module
