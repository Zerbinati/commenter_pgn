Imports System.Threading

Module modMain

    Public moteur As String
    Public moteur_court As String
    Public moteurEntete As String
    Public pgnextract As String
    Public mode As String
    Public duree_sec As Integer
    Public prof_fixe As Integer
    Public prof_actuelle As String
    Public ecoule_actuel As String
    Public dureeMoyProf As Integer
    Public taches As Integer
    Public memoire As Integer
    Public fichierEXP As String
    Public fichierBIN As String
    Private priorite As Integer

    Public tacheTitre As Thread
    Public titre As String

    Sub Main()
        Dim fichierPGN As String, fichierINI As String, fichierREPRISE As String, fichierUCI As String, fichierCOM As String
        Dim tailleFichier As Long, cumul As Long
        Dim lecture As System.IO.TextReader, ligne As String
        Dim suiteFR As String, coupFR As String, suiteUCI As String, positionDepart As String, resultat As String
        Dim indexCoup As Integer, nbCoups As Integer, nbParties As Integer, totCoups As Integer, totProf As Integer, prof As Integer
        Dim bddEPD As String, pos As Integer, epd As String, nbIdem As Integer
        Dim listeEPD As String, epdBlanc As String, epdNoir As String, repetitionBlanc As Integer, repetitionNoir As Integer
        Dim nbParties_reprise As Integer, indexReprise As Integer, depart As Integer
        Dim i As Integer, tabLigne() As String, j As Integer
        Dim chaine As String, chaine_mem As String, epd_mem As String
        Dim tabChaine() As String, tabTmp() As String, tabEXP(0) As Byte, reponse As String, score As Integer

        If My.Computer.FileSystem.GetFileInfo(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile) & "\Documents\Visual Studio 2013\Projects\commenter_pgn\commenter_pgn\bin\Debug\commenter_pgn.exe").LastWriteTime > My.Computer.FileSystem.GetFileInfo(My.Application.Info.AssemblyName & ".exe").LastWriteTime Then
            MsgBox("Il existe une version plus récente de ce programme !", MsgBoxStyle.Information)
            End
        End If

        fichierPGN = Replace(Command(), """", "")
        If fichierPGN = "" Then
            End
        End If

        'chargement parametres
        moteur = "E:\JEUX\ARENA CHESS 3.5.1\Engines\Eman\20T Eman 8.30 x64 BMI2.exe"
        If My.Computer.Name = "PLEXI" Then
            moteur = "D:\JEUX\ARENA CHESS 3.5.1\Engines\Eman\20T Eman 8.30 x64 PCNT.exe"
        End If
        pgnextract = "pgn-extract.exe"
        mode = "bestmove"
        duree_sec = 600
        prof_fixe = 0
        prof_actuelle = ""
        ecoule_actuel = ""
        taches = cpu()
        memoire = 16384
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
                                Case "moteur"
                                    moteur = tabTmp(1)
                                Case "pgnextract"
                                    pgnextract = tabTmp(1)
                                Case "mode"
                                    If InStr(tabTmp(1), "search", CompareMethod.Text) > 0 _
                                    And InStr(tabTmp(1), "move", CompareMethod.Text) > 0 Then
                                        mode = "searchmoves"
                                    End If
                                Case "duree_sec"
                                    duree_sec = CInt(tabTmp(1))
                                    If duree_sec > 0 Then
                                        prof_fixe = 0
                                    End If
                                Case "prof_fixe"
                                    prof_fixe = CInt(tabTmp(1))
                                    If prof_fixe > 0 Then
                                        duree_sec = 0
                                    End If
                                Case "taches"
                                    taches = CInt(tabTmp(1))
                                Case "memoire"
                                    memoire = CInt(tabTmp(1))
                                Case "priorite"
                                    priorite = CInt(tabTmp(1))
                                Case Else

                            End Select
                        End If
                    End If
                Next
            End If
        End If
        My.Computer.FileSystem.WriteAllText(fichierINI, "moteur = " & moteur & vbCrLf _
                                                      & "pgnextract = " & pgnextract & vbCrLf _
                                                      & "mode = " & mode & " //bestmove : analyse the opponent best move, searchmoves : only analyse the le current move" & vbCrLf _
                                                      & "duree_sec = " & duree_sec & vbCrLf _
                                                      & "prof_fixe = " & prof_fixe & vbCrLf _
                                                      & "taches = " & taches & vbCrLf _
                                                      & "memoire = " & memoire & vbCrLf _
                                                      & "priorite = " & priorite & " //64 (idle), 16384 (below normal), 32 (normal), 32768 (above normal), 128 (high), 256 (realtime)" & vbCrLf, False)

        moteur_court = Replace(nomFichier(moteur), ".exe", "")

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
        If duree_sec > 0 Then
            If InStr(moteur_court, "eman", CompareMethod.Text) > 0 Then
                chaine = "eman_eval\" & duree_sec & "_sec.epd"
                If Not My.Computer.FileSystem.DirectoryExists("eman_eval") Then
                    My.Computer.FileSystem.CreateDirectory("eman_eval")
                End If
            ElseIf InStr(moteur_court, "brainlearn", CompareMethod.Text) > 0 Then
                chaine = "brainlearn_eval\" & duree_sec & "_sec.epd"
                If Not My.Computer.FileSystem.DirectoryExists("brainlearn_eval") Then
                    My.Computer.FileSystem.CreateDirectory("brainlearn_eval")
                End If
            ElseIf InStr(moteur_court, "hypnos", CompareMethod.Text) > 0 Then
                chaine = "hypnos_eval\" & duree_sec & "_sec.epd"
                If Not My.Computer.FileSystem.DirectoryExists("hypnos_eval") Then
                    My.Computer.FileSystem.CreateDirectory("hypnos_eval")
                End If
            ElseIf InStr(moteur_court, "judas", CompareMethod.Text) > 0 Then
                chaine = "judas_eval\" & duree_sec & "_sec.epd"
                If Not My.Computer.FileSystem.DirectoryExists("judas_eval") Then
                    My.Computer.FileSystem.CreateDirectory("judas_eval")
                End If
            ElseIf InStr(moteur_court, "stockfishmz", CompareMethod.Text) > 0 Then
                chaine = "stockfishmz_eval\" & duree_sec & "_sec.epd"
                If Not My.Computer.FileSystem.DirectoryExists("stockfishmz_eval") Then
                    My.Computer.FileSystem.CreateDirectory("stockfishmz_eval")
                End If
            Else
                chaine = duree_sec & "_sec.epd"
            End If

            If My.Computer.FileSystem.FileExists(chaine) Then
                Console.Write("Loading " & nomFichier(chaine) & "... ")
                bddEPD = My.Computer.FileSystem.ReadAllText(chaine)
                Console.WriteLine("OK")
            End If
        ElseIf prof_fixe > 0 Then
            If InStr(moteur_court, "eman", CompareMethod.Text) > 0 Then
                chaine = "eman_eval\" & "P" & prof_fixe & ".epd"
                If Not My.Computer.FileSystem.DirectoryExists("eman_eval") Then
                    My.Computer.FileSystem.CreateDirectory("eman_eval")
                End If
            ElseIf InStr(moteur_court, "brainlearn", CompareMethod.Text) > 0 Then
                chaine = "brainlearn_eval\" & "P" & prof_fixe & ".epd"
                If Not My.Computer.FileSystem.DirectoryExists("brainlearn_eval") Then
                    My.Computer.FileSystem.CreateDirectory("brainlearn_eval")
                End If
            ElseIf InStr(moteur_court, "hypnos", CompareMethod.Text) > 0 Then
                chaine = "hypnos_eval\" & "P" & prof_fixe & ".epd"
                If Not My.Computer.FileSystem.DirectoryExists("hypnos_eval") Then
                    My.Computer.FileSystem.CreateDirectory("hypnos_eval")
                End If
            ElseIf InStr(moteur_court, "judas", CompareMethod.Text) > 0 Then
                chaine = "judas_eval\" & "P" & prof_fixe & ".epd"
                If Not My.Computer.FileSystem.DirectoryExists("judas_eval") Then
                    My.Computer.FileSystem.CreateDirectory("judas_eval")
                End If
            ElseIf InStr(moteur_court, "stockfishmz", CompareMethod.Text) > 0 Then
                chaine = "stockfishmz_eval\" & "P" & prof_fixe & ".epd"
                If Not My.Computer.FileSystem.DirectoryExists("stockfishmz_eval") Then
                    My.Computer.FileSystem.CreateDirectory("stockfishmz_eval")
                End If
            Else
                chaine = "P" & prof_fixe & ".epd"
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
                            If CInt(tabTmp(2)) = prof_fixe Then
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
            fichierEXP = "Eman.exp"
            If Not My.Computer.FileSystem.FileExists(fichierEXP) Then
                fichierEXP = My.Computer.Name & ".exp"
            End If

            moteurEntete = ""
            chargerMoteur(moteur, taches, memoire, priorite)
            Console.WriteLine("OK")

            If moteurEntete <> "" Then
                If InStr(moteur_court, "eman 6", CompareMethod.Text) > 0 And InStr(moteurEntete, "Collisions: 0", CompareMethod.Text) = 0 Then
                    moteurEntete = defragEXP(fichierEXP, 1, True, entree, sortie)
                ElseIf (InStr(moteur_court, "eman 7", CompareMethod.Text) > 0 Or InStr(moteur_court, "eman 8", CompareMethod.Text) > 0) And InStr(moteurEntete, "Duplicate moves: 0", CompareMethod.Text) = 0 Then
                    moteurEntete = defragEXP(fichierEXP, 4, True, entree, sortie)
                End If
                Console.WriteLine(moteurEntete)
            End If
        ElseIf InStr(moteur_court, "brainlearn", CompareMethod.Text) > 0 Then
            fichierBIN = Replace(moteur, nomFichier(moteur), "experience.bin")
            Console.WriteLine("Defrag " & nomFichier(fichierBIN) & "... ")
            Console.WriteLine(defragBIN(fichierBIN, 1))

            Console.Write("Loading " & moteur_court & "... ")
            chargerMoteur(moteur, taches, memoire, priorite)
            Console.WriteLine("OK")
        ElseIf InStr(moteur_court, "hypnos", CompareMethod.Text) > 0 Then
            Console.Write("Loading " & moteur_court & "... ")
            fichierEXP = "Hypnos.exp"
            If Not My.Computer.FileSystem.FileExists(fichierEXP) Then
                fichierEXP = My.Computer.Name & ".exp"
            End If

            moteurEntete = ""
            chargerMoteur(moteur, taches, memoire, priorite)
            Console.WriteLine("OK")

            If moteurEntete <> "" Then
                If InStr(moteurEntete, "Duplicate moves: 0", CompareMethod.Text) = 0 Then
                    moteurEntete = defragHypnos(moteur, fichierEXP, entree, sortie)
                End If
                Console.WriteLine(moteurEntete)
            End If
        ElseIf InStr(moteur_court, "judas", CompareMethod.Text) > 0 Then
            fichierBIN = Replace(moteur, nomFichier(moteur), "experience.bin")
            Console.WriteLine("Defrag " & nomFichier(fichierBIN) & "... ")
            Console.WriteLine(defragBIN(fichierBIN, 1))

            Console.Write("Loading " & moteur_court & "... ")
            chargerMoteur(moteur, taches, memoire, priorite)
            Console.WriteLine("OK")
        ElseIf InStr(moteur_court, "stockfishmz", CompareMethod.Text) > 0 Then
            Console.Write("Loading " & moteur_court & "... ")
            fichierEXP = "StockfishMZ.exp"
            If Not My.Computer.FileSystem.FileExists(fichierEXP) Then
                fichierEXP = My.Computer.Name & ".exp"
            End If

            moteurEntete = ""
            chargerMoteur(moteur, taches, memoire, priorite)
            Console.WriteLine("OK")

            If moteurEntete <> "" Then
                If InStr(moteurEntete, "Duplicate moves: 0", CompareMethod.Text) = 0 Then
                    moteurEntete = defragHypnos(moteur, fichierEXP, entree, sortie)
                End If
                Console.WriteLine(moteurEntete)
            End If
        Else
            Console.Write("Loading " & moteur_court & "... ")
            chargerMoteur(moteur, taches, memoire, priorite)
            Console.WriteLine("OK")
        End If

        cumul = 0
        nbParties = 0
        nbCoups = 0 'PlyCount
        totCoups = 0 'total coups cumulés
        totProf = 0
        moteurEntete = ""
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
                        Case "fichierPGN"
                            If tabTmp(1) <> fichierPGN Then
                                MsgBox("The resume file isn't attached to the PGN file." & vbCrLf _
                                     & "resume = " & nomFichier(tabTmp(1)) & vbCrLf _
                                     & "pgn file = " & nomFichier(fichierPGN), MsgBoxStyle.Exclamation)
                                End
                            End If

                        Case "nbParties"
                            nbParties_reprise = CInt(tabTmp(1))

                        Case "totCoups"
                            totCoups = CInt(tabTmp(1))

                        Case "totProf"
                            totProf = CInt(tabTmp(1))

                        Case Else

                    End Select
                End If
            Next
        End If

        titre = ""
        tacheTitre = New Thread(AddressOf afficherTitre)
        tacheTitre.Start()

        'ligne par ligne
        Do
            ligne = lecture.ReadLine()
            cumul = cumul + Len(ligne) + 2 'vbcrlf

            If ligne <> "" And InStr(ligne, "[") = 0 And InStr(ligne, "]") = 0 And InStr(ligne, """") = 0 Then
                nbParties = nbParties + 1
                If nbParties >= nbParties_reprise Then
                    My.Computer.FileSystem.WriteAllText(fichierREPRISE, "fichier = " & nomFichier(fichierPGN) & vbCrLf _
                                                                      & "nbParties = " & nbParties & vbCrLf _
                                                                      & "totCoups = " & totCoups & vbCrLf _
                                                                      & "totProf = " & totProf & vbCrLf, False)

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
                        moteurEntete = moteurEntete & chaine & vbCrLf
                    End If

                    chaine = "[Annotated """ & moteur_court & """]"
                    Console.WriteLine(chaine)
                    moteurEntete = moteurEntete & chaine & vbCrLf & vbCrLf

                    Do
                        suiteUCI = Trim(suiteUCI & " " & tabLigne(i)) 'suite de coups moteur
                        indexCoup = indexCoup + 1

                        If positionDepart = "" Then
                            epd = moteurEPD(moteur, suiteUCI)
                        Else
                            epd = moteurEPD(moteur, suiteUCI, positionDepart)
                        End If

                        coupFR = analyseCoups(tabLigne(i), suiteFR, positionDepart)

                        If indexCoup Mod 2 = 1 Then
                            Console.WriteLine()
                            Console.Write(Format(indexCoup / 2, "000") & ". " & StrDup(7 - Len(coupFR), " ") & coupFR)
                            'répétition
                            epdBlanc = epd.Substring(0, epd.IndexOf(" "))
                            listeEPD = listeEPD & epdBlanc & vbCrLf
                        Else
                            Console.Write(".... " & StrDup(7 - Len(coupFR), " ") & coupFR)
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
                                reponse = retourExperience(chaine_mem, suiteUCI.Substring(suiteUCI.LastIndexOf(" ") + 1), prof_fixe)
                                If reponse = "" And InStr(tabTmp(1), "M", CompareMethod.Text) = 0 Then
                                    'obtenir la position fen puis voir plainToEXP pour la structure d'une entrée dans fichierEXP
                                    If InStr(moteur_court, "eman 6", CompareMethod.Text) > 0 Then
                                        ReDim tabEXP(31)
                                    ElseIf InStr(moteur_court, "eman 7", CompareMethod.Text) > 0 Or InStr(moteur_court, "eman 8", CompareMethod.Text) > 0 _
                                        Or InStr(moteur_court, "hypnos", CompareMethod.Text) > 0 Or InStr(moteur_court, "stockfishmz", CompareMethod.Text) > 0 Then
                                        ReDim tabEXP(23)
                                    End If

                                    If positionDepart = "" Then
                                        epd_mem = moteurEPD(moteur, epd_mem)
                                    Else
                                        epd_mem = moteurEPD(moteur, epd_mem, positionDepart)
                                    End If

                                    score = CInt(CSng(Replace(tabTmp(1), ".", ",")) * 100)
                                    If InStr(epd_mem, " b ") > 0 Then
                                        score = -score
                                    End If
                                    entreeEXP(tabEXP, epd_mem, suiteUCI.Substring(suiteUCI.LastIndexOf(" ") + 1), score, 100, prof, 1, entree, sortie, moteur_court)

                                    My.Computer.FileSystem.WriteAllBytes(fichierEXP, tabEXP, True)
                                End If
                            End If
                        Else
                            If depart = 0 Then
                                depart = Environment.TickCount
                                indexReprise = indexCoup - 1
                                If totCoups > 0 Then
                                    If duree_sec > 0 Then
                                        titre = My.Computer.Name & " @ " & Format(cumul / tailleFichier, "0.00%") & ", game " & nbParties & " (" & resultat & ") @ " & Format((indexCoup - 1) / nbCoups, "0%") & " (" & Format(nbCoups - (indexCoup - 1)) & "), " & Format(DateAdd(DateInterval.Second, (nbCoups - (indexCoup - 1)) * duree_sec, Now), "dd/MM/yy HH:mm:ss") & ", " & "avg. D" & Format(totProf / totCoups, "0") & " @ " & duree_sec & " sec/pos (" & Trim(Format(totCoups, "# ##0")) & ")"
                                    ElseIf prof_fixe > 0 Then
                                        titre = My.Computer.Name & " @ " & Format(cumul / tailleFichier, "0.00%") & ", game " & nbParties & " (" & resultat & ") @ " & Format((indexCoup - 1) / nbCoups, "0%") & " (" & Format(nbCoups - (indexCoup - 1)) & "), " & Format(DateAdd(DateInterval.Second, (nbCoups - (indexCoup - 1)) * dureeMoyProf, Now), "dd/MM/yy HH:mm:ss") & ", D" & Format(prof_fixe, "0") & " (" & Trim(Format(totCoups, "# ##0")) & ")"
                                    End If
                                Else
                                    If duree_sec > 0 Then
                                        titre = My.Computer.Name & " @ " & Format(cumul / tailleFichier, "0.00%") & ", game " & nbParties & " (" & resultat & ") @ " & Format((indexCoup - 1) / nbCoups, "0%") & " (" & Format(nbCoups - (indexCoup - 1)) & "), " & Format(DateAdd(DateInterval.Second, (nbCoups - (indexCoup - 1)) * duree_sec, Now), "dd/MM/yy HH:mm:ss") & ", avg. D0 @ " & duree_sec & " sec/pos (" & Trim(Format(totCoups, "# ##0")) & ")"
                                    ElseIf prof_fixe > 0 Then
                                        titre = My.Computer.Name & " @ " & Format(cumul / tailleFichier, "0.00%") & ", game " & nbParties & " (" & resultat & ") @ " & Format((indexCoup - 1) / nbCoups, "0%") & " (" & Format(nbCoups - (indexCoup - 1)) & "), " & Format(DateAdd(DateInterval.Second, (nbCoups - (indexCoup - 1)) * dureeMoyProf, Now), "dd/MM/yy HH:mm:ss") & ", D" & Format(prof_fixe, "0") & " (" & Trim(Format(totCoups, "# ##0")) & ")"
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
                                    chaine_mem = analyseSuite(suiteUCI, duree_sec, prof_fixe, positionDepart)
                                End If
                            Else
                                chaine_mem = analyseSuite(suiteUCI, duree_sec, prof_fixe, positionDepart)
                            End If

                            'maj Eman.exp

                            If InStr(moteur_court, "eman", CompareMethod.Text) > 0 Or InStr(moteur_court, "brainlearn", CompareMethod.Text) > 0 _
                            Or InStr(moteur_court, "hypnos", CompareMethod.Text) > 0 Or InStr(moteur_court, "stockfishmz", CompareMethod.Text) > 0 Then
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

                            If duree_sec > 0 Then
                                If InStr(moteur_court, "eman", CompareMethod.Text) > 0 Then
                                    My.Computer.FileSystem.WriteAllText("eman_eval\" & duree_sec & "_sec.epd", chaine_mem & vbCrLf, True)
                                ElseIf InStr(moteur_court, "brainlearn", CompareMethod.Text) > 0 Then
                                    My.Computer.FileSystem.WriteAllText("brainlearn_eval\" & duree_sec & "_sec.epd", chaine_mem & vbCrLf, True)
                                ElseIf InStr(moteur_court, "hypnos", CompareMethod.Text) > 0 Then
                                    My.Computer.FileSystem.WriteAllText("hypnos_eval\" & duree_sec & "_sec.epd", chaine_mem & vbCrLf, True)
                                ElseIf InStr(moteur_court, "judas", CompareMethod.Text) > 0 Then
                                    My.Computer.FileSystem.WriteAllText("judas_eval\" & duree_sec & "_sec.epd", chaine_mem & vbCrLf, True)
                                ElseIf InStr(moteur_court, "stockfishmz", CompareMethod.Text) > 0 Then
                                    My.Computer.FileSystem.WriteAllText("stockfishmz_eval\" & duree_sec & "_sec.epd", chaine_mem & vbCrLf, True)
                                Else
                                    My.Computer.FileSystem.WriteAllText(duree_sec & "_sec.epd", chaine_mem & vbCrLf, True)
                                End If
                            ElseIf prof_fixe > 0 Then
                                If InStr(moteur_court, "eman", CompareMethod.Text) > 0 Then
                                    My.Computer.FileSystem.WriteAllText("eman_eval\" & "P" & prof_fixe & ".epd", chaine_mem & vbCrLf, True)
                                ElseIf InStr(moteur_court, "brainlearn", CompareMethod.Text) > 0 Then
                                    My.Computer.FileSystem.WriteAllText("brainlearn_eval\" & "P" & prof_fixe & ".epd", chaine_mem & vbCrLf, True)
                                ElseIf InStr(moteur_court, "hypnos", CompareMethod.Text) > 0 Then
                                    My.Computer.FileSystem.WriteAllText("hypnos_eval\" & "P" & prof_fixe & ".epd", chaine_mem & vbCrLf, True)
                                ElseIf InStr(moteur_court, "judas", CompareMethod.Text) > 0 Then
                                    My.Computer.FileSystem.WriteAllText("judas_eval\" & "P" & prof_fixe & ".epd", chaine_mem & vbCrLf, True)
                                ElseIf InStr(moteur_court, "stockfishmz", CompareMethod.Text) > 0 Then
                                    My.Computer.FileSystem.WriteAllText("stockfishmz_eval\" & "P" & prof_fixe & ".epd", chaine_mem & vbCrLf, True)
                                Else
                                    My.Computer.FileSystem.WriteAllText("P" & prof_fixe & ".epd", chaine_mem & vbCrLf, True)
                                End If
                            End If
                        End If
                        totCoups = totCoups + 1
                        totProf = totProf + prof

                        'on affiche la progression
                        Console.Write(chaine)
                        If InStr(moteur_court, "eman", CompareMethod.Text) > 0 Then
                            chaine_mem = emanQuality(suiteUCI, suiteFR, positionDepart)
                            If chaine_mem <> "" Then
                                If coupFR = Trim(chaine_mem.Substring(0, chaine_mem.IndexOf(" {"))) Then
                                    nbIdem = nbIdem + 1
                                End If
                            End If
                            chaine_mem = " ||| exp : " & chaine_mem & " ||| idem : " & Format(nbIdem / indexCoup, "00%")
                            Console.Write(chaine_mem)
                        ElseIf InStr(moteur_court, "brainlearn", CompareMethod.Text) > 0 Then
                            chaine_mem = brainlearnQuality(moteur, fichierBIN, suiteUCI, suiteFR, positionDepart)
                            If chaine_mem <> "" Then
                                If coupFR = Trim(chaine_mem.Substring(0, chaine_mem.IndexOf(" {"))) Then
                                    nbIdem = nbIdem + 1
                                End If
                            End If
                            chaine_mem = " ||| bin : " & chaine_mem & " ||| idem : " & Format(nbIdem / indexCoup, "00%")
                            Console.Write(chaine_mem)
                        ElseIf InStr(moteur_court, "hypnos", CompareMethod.Text) > 0 Then
                            chaine_mem = emanQuality(suiteUCI, suiteFR, positionDepart)
                            If chaine_mem <> "" Then
                                If coupFR = Trim(chaine_mem.Substring(0, chaine_mem.IndexOf(" {"))) Then
                                    nbIdem = nbIdem + 1
                                End If
                            End If
                            chaine_mem = " ||| exp : " & chaine_mem & " ||| idem : " & Format(nbIdem / indexCoup, "00%")
                            Console.Write(chaine_mem)
                        ElseIf InStr(moteur_court, "judas", CompareMethod.Text) > 0 Then
                            chaine_mem = brainlearnQuality(moteur, fichierBIN, suiteUCI, suiteFR, positionDepart)
                            If chaine_mem <> "" Then
                                If coupFR = Trim(chaine_mem.Substring(0, chaine_mem.IndexOf(" {"))) Then
                                    nbIdem = nbIdem + 1
                                End If
                            End If
                            chaine_mem = " ||| bin : " & chaine_mem & " ||| idem : " & Format(nbIdem / indexCoup, "00%")
                            Console.Write(chaine_mem)
                        ElseIf InStr(moteur_court, "stockfishmz", CompareMethod.Text) > 0 Then
                            chaine_mem = emanQuality(suiteUCI, suiteFR, positionDepart)
                            If chaine_mem <> "" Then
                                If coupFR = Trim(chaine_mem.Substring(0, chaine_mem.IndexOf(" {"))) Then
                                    nbIdem = nbIdem + 1
                                End If
                            End If
                            chaine_mem = " ||| exp : " & chaine_mem & " ||| idem : " & Format(nbIdem / indexCoup, "00%")
                            Console.Write(chaine_mem)
                        End If
                        Console.WriteLine()

                        If depart = 0 Then
                            If duree_sec > 0 Then
                                titre = My.Computer.Name & " @ " & Format(cumul / tailleFichier, "0.00%") & ", game " & nbParties & " (" & resultat & ") @ " & Format(indexCoup / nbCoups, "0%") & " (" & Format(nbCoups - indexCoup) & "), avg. D" & Format(totProf / totCoups, "0") & " @ " & duree_sec & " sec/pos (" & Trim(Format(totCoups, "# ##0")) & ")"
                            ElseIf prof_fixe > 0 Then
                                titre = My.Computer.Name & " @ " & Format(cumul / tailleFichier, "0.00%") & ", game " & nbParties & " (" & resultat & ") @ " & Format(indexCoup / nbCoups, "0%") & " (" & Format(nbCoups - indexCoup) & "), D" & prof_fixe & " (" & Trim(Format(totCoups, "# ##0")) & ")"
                            End If
                        Else
                            If duree_sec > 0 Then
                                titre = My.Computer.Name & " @ " & Format(cumul / tailleFichier, "0.00%") & ", game " & nbParties & " (" & resultat & ") @ " & Format(indexCoup / nbCoups, "0%") & " (" & Format(nbCoups - indexCoup) & "), " & heureFin(depart, indexCoup, nbCoups, indexReprise, True) & ", " & "avg. D" & Format(totProf / totCoups, "0") & " @ " & duree_sec & " sec/pos (" & Trim(Format(totCoups, "# ##0")) & ")"
                            ElseIf prof_fixe > 0 Then
                                titre = My.Computer.Name & " @ " & Format(cumul / tailleFichier, "0.00%") & ", game " & nbParties & " (" & resultat & ") @ " & Format(indexCoup / nbCoups, "0%") & " (" & Format(nbCoups - indexCoup) & "), " & heureFin(depart, indexCoup, nbCoups, indexReprise, True) & ", D" & prof_fixe & " (" & Trim(Format(totCoups, "# ##0")) & ")"
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

                    chaine = moteurEntete & coupsEN(suiteFR) & tabLigne(i) & vbCrLf & vbCrLf

                    My.Computer.FileSystem.WriteAllText(fichierCOM, chaine, True)

                    Threading.Thread.Sleep(250)
                ElseIf nbParties_reprise > 0 Then
                    Console.Title = "resume @ " & Format(nbParties / nbParties_reprise, "0%")
                End If

                Console.Clear()
                moteurEntete = ""
                positionDepart = ""
                resultat = ""
                nbCoups = 0
                prof_actuelle = ""
                ecoule_actuel = ""

            ElseIf ligne <> "" Then

                moteurEntete = moteurEntete & ligne & vbCrLf
                If InStr(ligne, "[PlyCount ", CompareMethod.Text) > 0 Then
                    nbCoups = CInt(Replace(Replace(ligne, "[PlyCount """, "", , , CompareMethod.Text), """]", ""))
                ElseIf InStr(ligne, "[FEN ", CompareMethod.Text) > 0 Then
                    positionDepart = Replace(Replace(ligne, "[FEN """, "", , , CompareMethod.Text), """]", "")
                ElseIf InStr(ligne, "[Result ", CompareMethod.Text) > 0 Then
                    resultat = Replace(Replace(ligne, "[Result """, "", , , CompareMethod.Text), """]", "")
                End If

                's'il n'y a pas de reprise ou s'il y a une reprise mais qu'on l'a atteinte voir dépassée
                If 0 = nbParties_reprise Or nbParties_reprise <= nbParties + 1 Then
                    'on affiche
                    Console.WriteLine(ligne)
                End If

            End If
        Loop Until ligne Is Nothing

        lecture.Close()
        dechargerMoteur()
        tacheTitre.Abort()

        If My.Computer.FileSystem.FileExists(fichierUCI) Then
            My.Computer.FileSystem.DeleteFile(fichierUCI)
        End If

        If My.Computer.FileSystem.FileExists(fichierREPRISE) Then
            My.Computer.FileSystem.DeleteFile(fichierREPRISE, FileIO.UIOption.OnlyErrorDialogs, FileIO.RecycleOption.SendToRecycleBin)
        End If

        If duree_sec > 0 Then
            titre = My.Computer.Name & " @ " & Format(cumul / tailleFichier, "0%") & ", " & nbParties & " games, avg. D" & Format(totProf / totCoups, "0") & " @ " & duree_sec & " sec/pos (" & Trim(Format(totCoups, "# ##0")) & ")"
        ElseIf prof_fixe > 0 Then
            titre = My.Computer.Name & " @ " & Format(cumul / tailleFichier, "0%") & ", " & nbParties & " games, D" & prof_fixe & " (" & Trim(Format(totCoups, "# ##0")) & ")"
        End If
        Console.Title = titre
        Console.WriteLine("Press ENTER to close this window.")
        Console.ReadLine()

    End Sub

    Public Sub afficherTitre()
        While True
            If titre <> "" And Console.Title <> titre & ", " & prof_actuelle & " @ " & ecoule_actuel Then
                If prof_actuelle <> "" Then
                    If InStr(prof_actuelle, " ") = 0 Then
                        Console.Title = titre & ", " & prof_actuelle & " @ " & ecoule_actuel
                    End If
                Else
                    Console.Title = titre
                End If
            End If
            System.Threading.Thread.Sleep(250)
        End While
    End Sub

End Module
