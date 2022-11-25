Imports System.Threading

Module modMain

    Public engine As String
    Public moteur_court As String
    Public headerEngine As String
    Public pgnextract As String
    Public mode As String
    Public delay_sec As Integer
    Public fixed_depth As Integer
    Public currentDepth As String
    Public ecoule_actuel As String
    Public dureeMoyProf As Integer
    Public nbThreads As Integer
    Public hashSize As Integer
    Public EXPFile As String
    Public BINFile As String
    Private priorite As Integer

    Public titleThread As Thread
    Public title As String

    Sub Main()
        Dim fichierPGN As String, fichierINI As String, fichierREPRISE As String, fichierUCI As String, fichierCOM As String
        Dim tailleFichier As Long, cumul As Long
        Dim lecture As System.IO.TextReader, ligne As String
        Dim suiteFR As String, coupFR As String, suiteUCI As String, positionDepart As String, resultat As String
        Dim indexCoup As Integer, nbCoups As Integer, nbGames As Integer, totMoves As Integer, totDepth As Integer, prof As Integer
        Dim bddEPD As String, pos As Integer, epd As String, nbIdem As Integer
        Dim listeEPD As String, epdBlanc As String, epdNoir As String, repetitionBlanc As Integer, repetitionNoir As Integer
        Dim nbParties_reprise As Integer, indexReprise As Integer, depart As Integer
        Dim i As Integer, tabLigne() As String, j As Integer
        Dim chaine As String, chaine_mem As String, epd_mem As String
        Dim tabChaine() As String, tabTmp() As String, tabEXP(0) As Byte, reponse As String, score As Integer

        fichierPGN = Replace(Command(), """", "")
        If fichierPGN = "" Then
            End
        End If

        'chargement parametres
        engine = "BrainLearn.exe"
        pgnextract = "pgn-extract.exe"
        mode = "bestmove"
        delay_sec = 600
        fixed_depth = 0
        currentDepth = ""
        ecoule_actuel = ""
        nbThreads = cpu()
        hashSize = 16384
        priorite = 64
        fichierINI = My.Computer.Name & ".ini"
        If My.Computer.FileSystem.FileExists(fichierINI) Then
            chaine = My.Computer.FileSystem.ReadAllText(fichierINI)
            If chaine <> "" And InStr(chaine, vbCrLf) > 0 Then
                tabChaine = Split(chaine, vbCrLf)
                For i = 0 To UBound(tabChaine)
                    If tabChaine(i) <> "" And InStr(tabChaine(i), " = ") > 0 Then
                        tabTmp = Split(tabChaine(i), " = ")
                        If tabTmp(0) <> "" And tabTmp(1) <> "" Then
                            If InStr(tabTmp(1), "//") > 0 Then
                                tabTmp(1) = Trim(gauche(tabTmp(1), tabTmp(1).IndexOf("//") - 1))
                            End If
                            Select Case tabTmp(0)
                                Case "moteur", "engine"
                                    engine = tabTmp(1)
                                Case "pgnextract"
                                    pgnextract = tabTmp(1)
                                Case "mode"
                                    If InStr(tabTmp(1), "search", CompareMethod.Text) > 0 _
                                    And InStr(tabTmp(1), "move", CompareMethod.Text) > 0 Then
                                        mode = "searchmoves"
                                    End If
                                Case "duree_sec", "delay_sec"
                                    delay_sec = CInt(tabTmp(1))
                                    If delay_sec > 0 Then
                                        fixed_depth = 0
                                    End If
                                Case "prof_fixe", "fixed_depth"
                                    fixed_depth = CInt(tabTmp(1))
                                    If fixed_depth > 0 Then
                                        delay_sec = 0
                                    End If
                                Case "taches", "threads"
                                    nbThreads = CInt(tabTmp(1))
                                Case "memoire", "hash"
                                    hashSize = CInt(tabTmp(1))
                                Case "priorite"
                                    priorite = CInt(tabTmp(1))
                                Case Else

                            End Select
                        End If
                    End If
                Next
            End If
        End If
        My.Computer.FileSystem.WriteAllText(fichierINI, "engine = " & engine & vbCrLf _
                                                      & "pgnextract = " & pgnextract & vbCrLf _
                                                      & "mode = " & mode & " //bestmove : analyse the opponent best move, searchmoves : only analyse the le current move" & vbCrLf _
                                                      & "delay_sec = " & delay_sec & vbCrLf _
                                                      & "fixed_depth = " & fixed_depth & vbCrLf _
                                                      & "threads = " & nbThreads & vbCrLf _
                                                      & "hash = " & hashSize & vbCrLf _
                                                      & "priorite = " & priorite & " //64 (idle), 16384 (below normal), 32 (normal), 32768 (above normal), 128 (high), 256 (realtime)" & vbCrLf, False)

        moteur_court = Replace(nomFichier(engine), ".exe", "")

        fichierUCI = Replace(fichierPGN, ".pgn", "_uci.pgn")
        If My.Computer.FileSystem.FileExists(fichierUCI) Then
            My.Computer.FileSystem.DeleteFile(fichierUCI)
        End If
        Console.Write("pgn-extract... ")
        pgnUCI(pgnextract, fichierPGN, "_uci", priorite)
        Console.WriteLine("OK")
        Try
            tailleFichier = FileLen(fichierUCI)
        Catch ex As Exception
            End
        End Try

        fichierCOM = Replace(fichierPGN, ".pgn", "_annotated.pgn")

        bddEPD = ""
        dureeMoyProf = 1
        pos = 0
        If delay_sec > 0 Then
            If InStr(moteur_court, "eman", CompareMethod.Text) > 0 Then
                chaine = "eman_eval\" & delay_sec & "_sec.epd"
                If Not My.Computer.FileSystem.DirectoryExists("eman_eval") Then
                    My.Computer.FileSystem.CreateDirectory("eman_eval")
                End If
            ElseIf InStr(moteur_court, "brainlearn", CompareMethod.Text) > 0 Then
                chaine = "brainlearn_eval\" & delay_sec & "_sec.epd"
                If Not My.Computer.FileSystem.DirectoryExists("brainlearn_eval") Then
                    My.Computer.FileSystem.CreateDirectory("brainlearn_eval")
                End If
            ElseIf InStr(moteur_court, "hypnos", CompareMethod.Text) > 0 Then
                chaine = "hypnos_eval\" & delay_sec & "_sec.epd"
                If Not My.Computer.FileSystem.DirectoryExists("hypnos_eval") Then
                    My.Computer.FileSystem.CreateDirectory("hypnos_eval")
                End If
            ElseIf InStr(moteur_court, "judas", CompareMethod.Text) > 0 Then
                chaine = "judas_eval\" & delay_sec & "_sec.epd"
                If Not My.Computer.FileSystem.DirectoryExists("judas_eval") Then
                    My.Computer.FileSystem.CreateDirectory("judas_eval")
                End If
            ElseIf InStr(moteur_court, "stockfishmz", CompareMethod.Text) > 0 Then
                chaine = "stockfishmz_eval\" & delay_sec & "_sec.epd"
                If Not My.Computer.FileSystem.DirectoryExists("stockfishmz_eval") Then
                    My.Computer.FileSystem.CreateDirectory("stockfishmz_eval")
                End If
            ElseIf InStr(moteur_court, "aurora", CompareMethod.Text) > 0 Then
                chaine = "aurora_eval\" & delay_sec & "_sec.epd"
                If Not My.Computer.FileSystem.DirectoryExists("aurora_eval") Then
                    My.Computer.FileSystem.CreateDirectory("aurora_eval")
                End If
            ElseIf InStr(moteur_court, "shashchess", CompareMethod.Text) > 0 Then
                chaine = "shashchess_eval\" & delay_sec & "_sec.epd"
                If Not My.Computer.FileSystem.DirectoryExists("shashchess_eval") Then
                    My.Computer.FileSystem.CreateDirectory("shashchess_eval")
                End If
            Else
                chaine = delay_sec & "_sec.epd"
            End If

            If My.Computer.FileSystem.FileExists(chaine) Then
                Console.Write("Loading " & nomFichier(chaine) & "... ")
                bddEPD = My.Computer.FileSystem.ReadAllText(chaine)
                Console.WriteLine("OK")
            End If
        ElseIf fixed_depth > 0 Then
            If InStr(moteur_court, "eman", CompareMethod.Text) > 0 Then
                chaine = "eman_eval\" & "P" & fixed_depth & ".epd"
                If Not My.Computer.FileSystem.DirectoryExists("eman_eval") Then
                    My.Computer.FileSystem.CreateDirectory("eman_eval")
                End If
            ElseIf InStr(moteur_court, "brainlearn", CompareMethod.Text) > 0 Then
                chaine = "brainlearn_eval\" & "P" & fixed_depth & ".epd"
                If Not My.Computer.FileSystem.DirectoryExists("brainlearn_eval") Then
                    My.Computer.FileSystem.CreateDirectory("brainlearn_eval")
                End If
            ElseIf InStr(moteur_court, "hypnos", CompareMethod.Text) > 0 Then
                chaine = "hypnos_eval\" & "P" & fixed_depth & ".epd"
                If Not My.Computer.FileSystem.DirectoryExists("hypnos_eval") Then
                    My.Computer.FileSystem.CreateDirectory("hypnos_eval")
                End If
            ElseIf InStr(moteur_court, "judas", CompareMethod.Text) > 0 Then
                chaine = "judas_eval\" & "P" & fixed_depth & ".epd"
                If Not My.Computer.FileSystem.DirectoryExists("judas_eval") Then
                    My.Computer.FileSystem.CreateDirectory("judas_eval")
                End If
            ElseIf InStr(moteur_court, "stockfishmz", CompareMethod.Text) > 0 Then
                chaine = "stockfishmz_eval\" & "P" & fixed_depth & ".epd"
                If Not My.Computer.FileSystem.DirectoryExists("stockfishmz_eval") Then
                    My.Computer.FileSystem.CreateDirectory("stockfishmz_eval")
                End If
            ElseIf InStr(moteur_court, "aurora", CompareMethod.Text) > 0 Then
                chaine = "aurora_eval\" & "P" & fixed_depth & ".epd"
                If Not My.Computer.FileSystem.DirectoryExists("aurora_eval") Then
                    My.Computer.FileSystem.CreateDirectory("aurora_eval")
                End If
            ElseIf InStr(moteur_court, "shashchess", CompareMethod.Text) > 0 Then
                chaine = "shashchess_eval\" & "P" & fixed_depth & ".epd"
                If Not My.Computer.FileSystem.DirectoryExists("shashchess_eval") Then
                    My.Computer.FileSystem.CreateDirectory("shashchess_eval")
                End If
            Else
                chaine = "P" & fixed_depth & ".epd"
            End If

            If My.Computer.FileSystem.FileExists(chaine) Then
                Console.Write("Loading " & nomFichier(chaine) & "... ")
                bddEPD = My.Computer.FileSystem.ReadAllText(chaine)
                tabChaine = Split(bddEPD, vbCrLf)
                dureeMoyProf = 0
                For i = 0 To UBound(tabChaine)
                    If nbCaracteres(tabChaine(i), ";") = 3 Then
                        tabTmp = Split(tabChaine(i), ";")
                        If tabTmp(2) <> "" And IsNumeric(tabTmp(2)) Then
                            If CInt(tabTmp(2)) = fixed_depth Then
                                dureeMoyProf = dureeMoyProf + CInt(tabTmp(3))
                                pos = pos + 1
                            End If
                        End If
                    End If
                Next
                If pos > 0 Then
                    dureeMoyProf = dureeMoyProf / pos
                End If
                Console.WriteLine("OK")
            End If
        End If

        lecture = My.Computer.FileSystem.OpenTextFileReader(fichierUCI)

        If InStr(moteur_court, "eman", CompareMethod.Text) > 0 Then
            Console.Write("Loading " & moteur_court & "... ")
            EXPFile = "Eman.exp"
            If Not My.Computer.FileSystem.FileExists(EXPFile) Then
                EXPFile = My.Computer.Name & ".exp"
            End If

            headerEngine = ""
            chargerMoteur(engine, nbThreads, hashSize, priorite)
            Console.WriteLine("OK")

            If headerEngine <> "" Then
                If InStr(moteur_court, "eman 6", CompareMethod.Text) > 0 And InStr(headerEngine, "Collisions: 0", CompareMethod.Text) = 0 Then
                    headerEngine = defragEXP(EXPFile, 1, True, entree, sortie)
                ElseIf (InStr(moteur_court, "eman 7", CompareMethod.Text) > 0 Or InStr(moteur_court, "eman 8", CompareMethod.Text) > 0) And InStr(headerEngine, "Duplicate moves: 0", CompareMethod.Text) = 0 Then
                    headerEngine = defragEXP(EXPFile, 4, True, entree, sortie)
                End If
                Console.WriteLine(headerEngine)
            End If
        ElseIf InStr(moteur_court, "brainlearn", CompareMethod.Text) > 0 Then
            BINFile = Replace(engine, nomFichier(engine), "experience.bin")
            Console.WriteLine("Defrag " & nomFichier(BINFile) & "... ")
            Console.WriteLine(defragBIN(BINFile, 1))

            Console.Write("Loading " & moteur_court & "... ")
            chargerMoteur(engine, nbThreads, hashSize, priorite)
            Console.WriteLine("OK")
        ElseIf InStr(moteur_court, "hypnos", CompareMethod.Text) > 0 Then
            Console.Write("Loading " & moteur_court & "... ")
            EXPFile = "Hypnos.exp"
            If Not My.Computer.FileSystem.FileExists(EXPFile) Then
                EXPFile = My.Computer.Name & ".exp"
            End If

            headerEngine = ""
            chargerMoteur(engine, nbThreads, hashSize, priorite)
            Console.WriteLine("OK")

            If headerEngine <> "" Then
                If InStr(headerEngine, "Duplicate moves: 0", CompareMethod.Text) = 0 Then
                    headerEngine = defragHypnos(engine, EXPFile, entree, sortie)
                End If
                Console.WriteLine(headerEngine)
            End If
        ElseIf InStr(moteur_court, "judas", CompareMethod.Text) > 0 Then
            BINFile = Replace(engine, nomFichier(engine), "experience.bin")
            Console.WriteLine("Defrag " & nomFichier(BINFile) & "... ")
            Console.WriteLine(defragBIN(BINFile, 1))

            Console.Write("Loading " & moteur_court & "... ")
            chargerMoteur(engine, nbThreads, hashSize, priorite)
            Console.WriteLine("OK")
        ElseIf InStr(moteur_court, "stockfishmz", CompareMethod.Text) > 0 Then
            Console.Write("Loading " & moteur_court & "... ")
            EXPFile = "StockfishMZ.exp"
            If Not My.Computer.FileSystem.FileExists(EXPFile) Then
                EXPFile = My.Computer.Name & ".exp"
            End If

            headerEngine = ""
            chargerMoteur(engine, nbThreads, hashSize, priorite)
            Console.WriteLine("OK")

            If headerEngine <> "" Then
                If InStr(headerEngine, "Duplicate moves: 0", CompareMethod.Text) = 0 Then
                    headerEngine = defragHypnos(engine, EXPFile, entree, sortie)
                End If
                Console.WriteLine(headerEngine)
            End If
        ElseIf InStr(moteur_court, "shashchess", CompareMethod.Text) > 0 Then
            BINFile = Replace(engine, nomFichier(engine), "experience.bin")
            Console.WriteLine("Defrag " & nomFichier(BINFile) & "... ")
            Console.WriteLine(defragBIN(BINFile, 1))

            Console.Write("Loading " & moteur_court & "... ")
            chargerMoteur(engine, nbThreads, hashSize, priorite)
            Console.WriteLine("OK")
        ElseIf InStr(moteur_court, "aurora", CompareMethod.Text) > 0 Then
            Console.Write("Loading " & moteur_court & "... ")
            EXPFile = "Sarona.exp"
            If Not My.Computer.FileSystem.FileExists(EXPFile) Then
                EXPFile = My.Computer.Name & ".exp"
            End If

            headerEngine = ""
            chargerMoteur(engine, nbThreads, hashSize, priorite)
            Console.WriteLine("OK")

            If headerEngine <> "" Then
                If InStr(headerEngine, "Duplicate moves: 0", CompareMethod.Text) = 0 Then
                    headerEngine = defragHypnos(engine, EXPFile, entree, sortie)
                End If
                Console.WriteLine(headerEngine)
            End If
        Else
            Console.Write("Loading " & moteur_court & "... ")
            chargerMoteur(engine, nbThreads, hashSize, priorite)
            Console.WriteLine("OK")
        End If

        cumul = 0
        nbGames = 0
        nbCoups = 0 'PlyCount
        totMoves = 0 'total coups cumulés
        totDepth = 0
        headerEngine = ""
        positionDepart = ""
        resultat = ""

        nbParties_reprise = 0
        fichierREPRISE = My.Computer.Name & "_reprise_" & mode & ".ini"
        If My.Computer.FileSystem.FileExists(fichierREPRISE) Then
            chaine = My.Computer.FileSystem.ReadAllText(fichierREPRISE)
            tabChaine = Split(chaine, vbCrLf)
            For i = 0 To UBound(tabChaine)
                If InStr(tabChaine(i), " = ") > 0 Then
                    tabTmp = Split(tabChaine(i), " = ")
                    Select Case tabTmp(0)
                        Case "fichier", "PGNFile"
                            If tabTmp(1) <> fichierPGN Then
                                MsgBox("The resume file isn't attached to the PGN file." & vbCrLf _
                                     & "resume = " & tabTmp(1) & vbCrLf _
                                     & "PGNFile = " & fichierPGN, MsgBoxStyle.Exclamation)
                                End
                            End If

                        Case "nbParties", "nbGames"
                            nbParties_reprise = CInt(tabTmp(1))

                        Case "totCoups", "totMoves"
                            totMoves = CInt(tabTmp(1))

                        Case "totProf", "totDepth"
                            totDepth = CInt(tabTmp(1))

                        Case Else

                    End Select
                End If
            Next
        End If

        title = ""
        titleThread = New Thread(AddressOf afficherTitre)
        titleThread.Start()

        'ligne par ligne
        Do
            ligne = lecture.ReadLine()
            cumul = cumul + Len(ligne) + 2 'vbcrlf

            If ligne <> "" And InStr(ligne, "[") = 0 And InStr(ligne, "]") = 0 And InStr(ligne, """") = 0 Then
                nbGames = nbGames + 1
                If nbGames >= nbParties_reprise Then
                    My.Computer.FileSystem.WriteAllText(fichierREPRISE, "PGNFile = " & fichierPGN & vbCrLf _
                                                                      & "nbGames = " & nbGames & vbCrLf _
                                                                      & "totMoves = " & totMoves & vbCrLf _
                                                                      & "totDepth = " & totDepth & vbCrLf, False)

                    'on réévalue après chaque coup moteur
                    suiteUCI = ""
                    suiteFR = ""
                    listeEPD = "" 'repetition
                    epdBlanc = "" 'repetition
                    epdNoir = "" 'repetition
                    indexCoup = 0
                    nbIdem = 0

                    nouvellePartie()

                    i = 0
                    indexReprise = 0
                    depart = 0

                    chaine = ""
                    tabLigne = Split(ligne, " ")
                    If nbCoups = 0 And tabLigne.Length - 1 > 0 Then
                        nbCoups = tabLigne.Length - 1
                        chaine = "[PlyCount """ & Format(nbCoups, "0") & """]"
                        Console.WriteLine(chaine)
                        headerEngine = headerEngine & chaine & vbCrLf
                    End If

                    chaine = "[Annotated """ & moteur_court & """]"
                    Console.WriteLine(chaine)
                    headerEngine = headerEngine & chaine & vbCrLf & vbCrLf

                    Do
                        suiteUCI = Trim(suiteUCI & " " & tabLigne(i)) 'suite de coups moteur
                        indexCoup = indexCoup + 1

                        If positionDepart = "" Then
                            epd = moteurEPD(engine, suiteUCI)
                        Else
                            epd = moteurEPD(engine, suiteUCI, positionDepart)
                        End If

                        coupFR = analyseCoups(tabLigne(i), suiteFR, positionDepart)

                        If indexCoup Mod 2 = 1 Then
                            Console.WriteLine()
                            Console.Write(Format(indexCoup / 2, "000") & ". " & StrDup(7 - Len(coupFR), " ") & coupsEN(coupFR))
                            'répétition
                            epdBlanc = epd.Substring(0, epd.IndexOf(" "))
                            listeEPD = listeEPD & epdBlanc & vbCrLf
                        Else
                            Console.Write(".... " & StrDup(7 - Len(coupFR), " ") & coupsEN(coupFR))
                            'répétition
                            epdNoir = epd.Substring(0, epd.IndexOf(" "))
                            listeEPD = listeEPD & epdNoir & vbCrLf
                        End If

                        pos = InStr(bddEPD, epd & ";")
                        prof = 0
                        'déjà analysé
                        If pos > 0 Then
                            tabTmp = Split(bddEPD.Substring(pos - 1, bddEPD.Substring(pos - 1).IndexOf(vbCrLf)), ";")
                            If InStr(tabTmp(1), "#") > 0 Then
                                chaine = tabTmp(1)
                            ElseIf InStr(tabTmp(1), "Pat") > 0 Then
                                chaine = " {" & tabTmp(1) & "}"
                            Else
                                prof = Int(tabTmp(2))
                                chaine = " {" & tabTmp(1) & "/" & tabTmp(2) & " " & tabTmp(3) & "}"
                            End If

                            'ce coup a déjà été analysé mais est-ce qu'il figure dans le fichierEXP ?
                            If InStr(moteur_court, "eman", CompareMethod.Text) > 0 Or InStr(moteur_court, "hypnos", CompareMethod.Text) > 0 Then
                                epd_mem = ""
                                If InStr(suiteUCI, " ") > 0 Then
                                    epd_mem = suiteUCI.Substring(0, suiteUCI.LastIndexOf(" "))
                                End If

                                chaine_mem = ""
                                If positionDepart = "" Then
                                    chaine_mem = "position startpos moves " & epd_mem
                                Else
                                    chaine_mem = "position fen " & positionDepart & " moves " & epd_mem
                                End If

                                'sinon il faudrait le concaténer à la suite du fichierEXP
                                reponse = retourExperience(chaine_mem, suiteUCI.Substring(suiteUCI.LastIndexOf(" ") + 1), fixed_depth)
                                If reponse = "" And InStr(tabTmp(1), "M", CompareMethod.Text) = 0 Then
                                    'obtenir la position fen puis voir plainToEXP pour la structure d'une entrée dans fichierEXP
                                    If InStr(moteur_court, "eman 6", CompareMethod.Text) > 0 Then
                                        ReDim tabEXP(31)
                                    ElseIf InStr(moteur_court, "eman 7", CompareMethod.Text) > 0 Or InStr(moteur_court, "eman 8", CompareMethod.Text) > 0 _
                                        Or InStr(moteur_court, "hypnos", CompareMethod.Text) > 0 Or InStr(moteur_court, "stockfishmz", CompareMethod.Text) > 0 _
                                        Or InStr(moteur_court, "aurora", CompareMethod.Text) > 0 Then
                                        ReDim tabEXP(23)
                                    End If

                                    If positionDepart = "" Then
                                        epd_mem = moteurEPD(engine, epd_mem)
                                    Else
                                        epd_mem = moteurEPD(engine, epd_mem, positionDepart)
                                    End If

                                    score = CInt(CSng(Replace(tabTmp(1), ".", ",")) * 100)
                                    If InStr(epd_mem, " b ") > 0 Then
                                        score = -score
                                    End If
                                    entreeEXP(tabEXP, epd_mem, suiteUCI.Substring(suiteUCI.LastIndexOf(" ") + 1), score, 100, prof, 1, entree, sortie, moteur_court)

                                    My.Computer.FileSystem.WriteAllBytes(EXPFile, tabEXP, True)
                                End If
                            End If
                        Else
                            If depart = 0 Then
                                depart = Environment.TickCount
                                indexReprise = indexCoup - 1
                                If totMoves > 0 Then
                                    If delay_sec > 0 Then
                                        title = My.Computer.Name & " @ " & Format(cumul / tailleFichier, "0.00%") & ", game " & nbGames & " (" & resultat & ") @ " & Format((indexCoup - 1) / nbCoups, "0%") & " (" & Format(nbCoups - (indexCoup - 1)) & "), " & Format(DateAdd(DateInterval.Second, (nbCoups - (indexCoup - 1)) * delay_sec, Now), "dd/MM/yy HH:mm:ss") & ", " & "avg. D" & Format(totDepth / totMoves, "0") & " @ " & delay_sec & " sec/pos (" & Trim(Format(totMoves, "# ##0")) & ")"
                                    ElseIf fixed_depth > 0 Then
                                        title = My.Computer.Name & " @ " & Format(cumul / tailleFichier, "0.00%") & ", game " & nbGames & " (" & resultat & ") @ " & Format((indexCoup - 1) / nbCoups, "0%") & " (" & Format(nbCoups - (indexCoup - 1)) & "), " & Format(DateAdd(DateInterval.Second, (nbCoups - (indexCoup - 1)) * dureeMoyProf, Now), "dd/MM/yy HH:mm:ss") & ", D" & Format(fixed_depth, "0") & " (" & Trim(Format(totMoves, "# ##0")) & ")"
                                    End If
                                Else
                                    If delay_sec > 0 Then
                                        title = My.Computer.Name & " @ " & Format(cumul / tailleFichier, "0.00%") & ", game " & nbGames & " (" & resultat & ") @ " & Format((indexCoup - 1) / nbCoups, "0%") & " (" & Format(nbCoups - (indexCoup - 1)) & "), " & Format(DateAdd(DateInterval.Second, (nbCoups - (indexCoup - 1)) * delay_sec, Now), "dd/MM/yy HH:mm:ss") & ", avg. D0 @ " & delay_sec & " sec/pos (" & Trim(Format(totMoves, "# ##0")) & ")"
                                    ElseIf fixed_depth > 0 Then
                                        title = My.Computer.Name & " @ " & Format(cumul / tailleFichier, "0.00%") & ", game " & nbGames & " (" & resultat & ") @ " & Format((indexCoup - 1) / nbCoups, "0%") & " (" & Format(nbCoups - (indexCoup - 1)) & "), " & Format(DateAdd(DateInterval.Second, (nbCoups - (indexCoup - 1)) * dureeMoyProf, Now), "dd/MM/yy HH:mm:ss") & ", D" & Format(fixed_depth, "0") & " (" & Trim(Format(totMoves, "# ##0")) & ")"
                                    End If
                                End If
                            End If

                            chaine_mem = ""
                            If indexCoup >= 6 Then
                                repetitionBlanc = nbChaines(listeEPD, epdBlanc)
                                repetitionNoir = nbChaines(listeEPD, epdNoir)
                                If (repetitionBlanc >= 2 And repetitionNoir = 3) Or (repetitionBlanc = 3 And repetitionNoir >= 2) Then
                                    chaine_mem = "3-fold"
                                Else
                                    chaine_mem = analyseSuite(suiteUCI, delay_sec, fixed_depth, positionDepart)
                                End If
                            Else
                                chaine_mem = analyseSuite(suiteUCI, delay_sec, fixed_depth, positionDepart)
                            End If

                            'maj Eman.exp

                            If InStr(moteur_court, "eman", CompareMethod.Text) > 0 Or InStr(moteur_court, "brainlearn", CompareMethod.Text) > 0 _
                            Or InStr(moteur_court, "hypnos", CompareMethod.Text) > 0 Or InStr(moteur_court, "stockfishmz", CompareMethod.Text) > 0 _
                            Or InStr(moteur_court, "aurora", CompareMethod.Text) > 0 Then
                                entree.WriteLine("ucinewgame")
                                entree.WriteLine("isready")
                                chaine = ""
                                While InStr(chaine, "readyok") = 0
                                    chaine = sortie.ReadLine
                                    Threading.Thread.Sleep(1)
                                End While
                            End If

                            If chaine_mem = "checkmated" Then
                                chaine = "#"
                                chaine_mem = epd & ";#"
                            ElseIf chaine_mem = "stalemated" Then
                                chaine = " {Pat}"
                                chaine_mem = epd & ";Pat"
                            ElseIf chaine_mem = "3-fold" Then
                                chaine = " { 0.00/0 0}"
                                chaine_mem = epd & "; 0.00;0;0"
                            ElseIf chaine_mem <> "" Then
                                tabTmp = Split(chaine_mem, " ")
                                For j = 0 To UBound(tabTmp)
                                    If tabTmp(j) = "depth" Then
                                        prof = Int(tabTmp(j + 1))
                                        Exit For
                                    End If
                                Next

                                If InStr(positionDepart, " b ", CompareMethod.Text) > 0 Then
                                    chaine = formatage(chaine_mem, indexCoup + 1)
                                Else
                                    chaine = formatage(chaine_mem, indexCoup)
                                End If

                                'maj 600_sec.epd
                                chaine_mem = Replace(chaine, "{", ";")
                                chaine_mem = Replace(chaine_mem, "/", ";")
                                chaine_mem = Replace(chaine_mem, " ", ";")
                                chaine_mem = Replace(chaine_mem, "}", "")
                                chaine_mem = Replace(chaine_mem, ";0.00", " 0.00")
                                chaine_mem = epd & chaine_mem

                                chaine = " " & chaine
                            End If
                            bddEPD = bddEPD & chaine_mem & vbCrLf

                            If delay_sec > 0 Then
                                If InStr(moteur_court, "eman", CompareMethod.Text) > 0 Then
                                    My.Computer.FileSystem.WriteAllText("eman_eval\" & delay_sec & "_sec.epd", chaine_mem & vbCrLf, True)
                                ElseIf InStr(moteur_court, "brainlearn", CompareMethod.Text) > 0 Then
                                    My.Computer.FileSystem.WriteAllText("brainlearn_eval\" & delay_sec & "_sec.epd", chaine_mem & vbCrLf, True)
                                ElseIf InStr(moteur_court, "hypnos", CompareMethod.Text) > 0 Then
                                    My.Computer.FileSystem.WriteAllText("hypnos_eval\" & delay_sec & "_sec.epd", chaine_mem & vbCrLf, True)
                                ElseIf InStr(moteur_court, "judas", CompareMethod.Text) > 0 Then
                                    My.Computer.FileSystem.WriteAllText("judas_eval\" & delay_sec & "_sec.epd", chaine_mem & vbCrLf, True)
                                ElseIf InStr(moteur_court, "stockfishmz", CompareMethod.Text) > 0 Then
                                    My.Computer.FileSystem.WriteAllText("stockfishmz_eval\" & delay_sec & "_sec.epd", chaine_mem & vbCrLf, True)
                                ElseIf InStr(moteur_court, "aurora", CompareMethod.Text) > 0 Then
                                    My.Computer.FileSystem.WriteAllText("aurora_eval\" & delay_sec & "_sec.epd", chaine_mem & vbCrLf, True)
                                ElseIf InStr(moteur_court, "shashchess", CompareMethod.Text) > 0 Then
                                    My.Computer.FileSystem.WriteAllText("shashchess_eval\" & delay_sec & "_sec.epd", chaine_mem & vbCrLf, True)
                                Else
                                    My.Computer.FileSystem.WriteAllText(delay_sec & "_sec.epd", chaine_mem & vbCrLf, True)
                                End If
                            ElseIf fixed_depth > 0 Then
                                If InStr(moteur_court, "eman", CompareMethod.Text) > 0 Then
                                    My.Computer.FileSystem.WriteAllText("eman_eval\" & "P" & fixed_depth & ".epd", chaine_mem & vbCrLf, True)
                                ElseIf InStr(moteur_court, "brainlearn", CompareMethod.Text) > 0 Then
                                    My.Computer.FileSystem.WriteAllText("brainlearn_eval\" & "P" & fixed_depth & ".epd", chaine_mem & vbCrLf, True)
                                ElseIf InStr(moteur_court, "hypnos", CompareMethod.Text) > 0 Then
                                    My.Computer.FileSystem.WriteAllText("hypnos_eval\" & "P" & fixed_depth & ".epd", chaine_mem & vbCrLf, True)
                                ElseIf InStr(moteur_court, "judas", CompareMethod.Text) > 0 Then
                                    My.Computer.FileSystem.WriteAllText("judas_eval\" & "P" & fixed_depth & ".epd", chaine_mem & vbCrLf, True)
                                ElseIf InStr(moteur_court, "stockfishmz", CompareMethod.Text) > 0 Then
                                    My.Computer.FileSystem.WriteAllText("stockfishmz_eval\" & "P" & fixed_depth & ".epd", chaine_mem & vbCrLf, True)
                                ElseIf InStr(moteur_court, "aurora", CompareMethod.Text) > 0 Then
                                    My.Computer.FileSystem.WriteAllText("aurora_eval\" & "P" & fixed_depth & ".epd", chaine_mem & vbCrLf, True)
                                ElseIf InStr(moteur_court, "shashchess", CompareMethod.Text) > 0 Then
                                    My.Computer.FileSystem.WriteAllText("shashchess_eval\" & "P" & fixed_depth & ".epd", chaine_mem & vbCrLf, True)
                                Else
                                    My.Computer.FileSystem.WriteAllText("P" & fixed_depth & ".epd", chaine_mem & vbCrLf, True)
                                End If
                            End If
                        End If
                        totMoves = totMoves + 1
                        totDepth = totDepth + prof

                        'on affiche la progression
                        Console.Write(chaine)
                        If InStr(moteur_court, "eman", CompareMethod.Text) > 0 _
                        Or InStr(moteur_court, "hypnos", CompareMethod.Text) > 0 _
                        Or InStr(moteur_court, "stockfishmz", CompareMethod.Text) > 0 _
                        Or InStr(moteur_court, "aurora", CompareMethod.Text) > 0 Then
                            chaine_mem = emanQuality(suiteUCI, suiteFR, positionDepart)
                            If chaine_mem <> "" Then
                                If coupFR = Trim(chaine_mem.Substring(0, chaine_mem.IndexOf(" {"))) Then
                                    nbIdem = nbIdem + 1
                                End If
                            End If
                            chaine_mem = " ||| exp : " & coupsEN(chaine_mem) & " ||| idem : " & Format(nbIdem / indexCoup, "00%")
                            Console.Write(chaine_mem)
                        ElseIf InStr(moteur_court, "brainlearn", CompareMethod.Text) > 0 _
                            Or InStr(moteur_court, "judas", CompareMethod.Text) > 0 _
                            Or InStr(moteur_court, "shashchess", CompareMethod.Text) > 0 Then
                            chaine_mem = brainlearnQuality(engine, BINFile, suiteUCI, suiteFR, positionDepart)
                            If chaine_mem <> "" Then
                                If coupFR = Trim(chaine_mem.Substring(0, chaine_mem.IndexOf(" {"))) Then
                                    nbIdem = nbIdem + 1
                                End If
                            End If
                            chaine_mem = " ||| bin : " & coupsEN(chaine_mem) & " ||| idem : " & Format(nbIdem / indexCoup, "00%")
                            Console.Write(chaine_mem)
                        End If
                        Console.WriteLine()

                        If depart = 0 Then
                            If delay_sec > 0 Then
                                title = My.Computer.Name & " @ " & Format(cumul / tailleFichier, "0.00%") & ", game " & nbGames & " (" & resultat & ") @ " & Format(indexCoup / nbCoups, "0%") & " (" & Format(nbCoups - indexCoup) & "), avg. D" & Format(totDepth / totMoves, "0") & " @ " & delay_sec & " sec/pos (" & Trim(Format(totMoves, "# ##0")) & ")"
                            ElseIf fixed_depth > 0 Then
                                title = My.Computer.Name & " @ " & Format(cumul / tailleFichier, "0.00%") & ", game " & nbGames & " (" & resultat & ") @ " & Format(indexCoup / nbCoups, "0%") & " (" & Format(nbCoups - indexCoup) & "), D" & fixed_depth & " (" & Trim(Format(totMoves, "# ##0")) & ")"
                            End If
                        Else
                            If delay_sec > 0 Then
                                title = My.Computer.Name & " @ " & Format(cumul / tailleFichier, "0.00%") & ", game " & nbGames & " (" & resultat & ") @ " & Format(indexCoup / nbCoups, "0%") & " (" & Format(nbCoups - indexCoup) & "), " & heureFin(depart, indexCoup, nbCoups, indexReprise, True) & ", " & "avg. D" & Format(totDepth / totMoves, "0") & " @ " & delay_sec & " sec/pos (" & Trim(Format(totMoves, "# ##0")) & ")"
                            ElseIf fixed_depth > 0 Then
                                title = My.Computer.Name & " @ " & Format(cumul / tailleFichier, "0.00%") & ", game " & nbGames & " (" & resultat & ") @ " & Format(indexCoup / nbCoups, "0%") & " (" & Format(nbCoups - indexCoup) & "), " & heureFin(depart, indexCoup, nbCoups, indexReprise, True) & ", D" & fixed_depth & " (" & Trim(Format(totMoves, "# ##0")) & ")"
                            End If
                        End If

                        If indexCoup Mod 2 = 1 Then
                            suiteFR = suiteFR & Format(indexCoup / 2, "0") & ". " & coupFR & chaine & " "
                        Else
                            suiteFR = suiteFR & coupFR & chaine & " "
                        End If

                        i = i + 1
                    Loop Until InStr(tabLigne(i), "/") > 0 Or InStr(tabLigne(i), "-") > 0 Or InStr(tabLigne(i), "*") > 0

                    Console.WriteLine()
                    Console.WriteLine(tabLigne(i))

                    chaine = headerEngine & coupsEN(suiteFR) & tabLigne(i) & vbCrLf & vbCrLf

                    My.Computer.FileSystem.WriteAllText(fichierCOM, chaine, True)

                    Threading.Thread.Sleep(250)
                ElseIf nbParties_reprise > 0 Then
                    Console.Title = "resume @ " & Format(nbGames / nbParties_reprise, "0%")
                End If

                Console.Clear()
                headerEngine = ""
                positionDepart = ""
                resultat = ""
                nbCoups = 0
                currentDepth = ""
                ecoule_actuel = ""

            ElseIf ligne <> "" Then

                headerEngine = headerEngine & ligne & vbCrLf
                If InStr(ligne, "[PlyCount ", CompareMethod.Text) > 0 Then
                    nbCoups = CInt(Replace(Replace(ligne, "[PlyCount """, "", , , CompareMethod.Text), """]", ""))
                ElseIf InStr(ligne, "[FEN ", CompareMethod.Text) > 0 Then
                    positionDepart = Replace(Replace(ligne, "[FEN """, "", , , CompareMethod.Text), """]", "")
                ElseIf InStr(ligne, "[Result ", CompareMethod.Text) > 0 Then
                    resultat = Replace(Replace(ligne, "[Result """, "", , , CompareMethod.Text), """]", "")
                End If

                's'il n'y a pas de reprise ou s'il y a une reprise mais qu'on l'a atteinte voir dépassée
                If 0 = nbParties_reprise Or nbParties_reprise <= nbGames + 1 Then
                    'on affiche
                    Console.WriteLine(ligne)
                End If

            End If
        Loop Until ligne Is Nothing

        lecture.Close()
        dechargerMoteur()
        titleThread.Abort()

        If My.Computer.FileSystem.FileExists(fichierUCI) Then
            My.Computer.FileSystem.DeleteFile(fichierUCI)
        End If

        If My.Computer.FileSystem.FileExists(fichierREPRISE) Then
            My.Computer.FileSystem.DeleteFile(fichierREPRISE, FileIO.UIOption.OnlyErrorDialogs, FileIO.RecycleOption.SendToRecycleBin)
        End If

        If delay_sec > 0 Then
            title = My.Computer.Name & " @ " & Format(cumul / tailleFichier, "0%") & ", " & nbGames & " games, avg. D" & Format(totDepth / totMoves, "0") & " @ " & delay_sec & " sec/pos (" & Trim(Format(totMoves, "# ##0")) & ")"
        ElseIf fixed_depth > 0 Then
            title = My.Computer.Name & " @ " & Format(cumul / tailleFichier, "0%") & ", " & nbGames & " games, D" & fixed_depth & " (" & Trim(Format(totMoves, "# ##0")) & ")"
        End If
        Console.Title = title
        Console.WriteLine("Press ENTER to close this window.")
        Console.ReadLine()

    End Sub

    Public Sub afficherTitre()
        While True
            If title <> "" And Console.Title <> title & ", " & currentDepth & " @ " & ecoule_actuel Then
                If currentDepth <> "" Then
                    If InStr(currentDepth, " ") = 0 Then
                        Console.Title = title & ", " & currentDepth & " @ " & ecoule_actuel
                    End If
                Else
                    Console.Title = title
                End If
            End If
            System.Threading.Thread.Sleep(250)
        End While
    End Sub

End Module
