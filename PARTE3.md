# PROGRAMA-ROUND-ROBBIN
PARTE 3

    Public Sub Procesar()

        Try


            Dim No_Procesos As Integer = fiprocesos() - 1

            If No_Procesos > 0 Then

                Dim estado As Integer = 0
                Dim cpu As Integer = 0
                Dim nombreProceso As String = ""
                Dim valresta As Integer = 0
                Dim color As String = ""
                Dim pendientevalor As Integer = 0
                Dim fila As Integer = 0
                Dim Columna As Integer = 0
                Dim Contador_Procesos_en_cero As Integer = 0

                'Encontra_ultima_Fila_ultima_Col()

                fila = Ultima_Fila
                Columna = Ultima_Columa

                While (d <= No_Procesos) AndAlso (estado = 0)

                    cpu = tblprocesos.Item(3, d).Value 'CUANTO USO DE CPU TIENE ASIGNADO
                    nombreProceso = tblprocesos.Item(0, d).Value 'NOMBRE DEL PROCESO

                    If cpu > 0 Then

                        'ASIGNA EL QUANTUM DISPONIBLE PARA ESE PROCESO Y DEJA LA DIFERENCIA PENDIENTE
                        valresta = Val(cpu) - txtQuantum.Value

                        'CUANDO EL VALOR ES MAYOR O IGUAL QUE 0
                        If valresta >= 0 Then

                            'ASIGNAMOS LO QUE QUEDO PENDIENTE
                            tblprocesos.Item(3, d).Value = valresta

                            'OBTENEMOS EL NOMBRE DEL COLOR PARA ESTE PROCESO
                            color = tblprocesos.Item(2, d).Value

                            'EL CICLO LLENA UNA CASILLA POR CADA UN QUANTUM
                            For i = 1 To txtQuantum.Value

                                dgridRoundRobin.Item(Columna, fila).Value = nombreProceso
                                pintar(color, fila, Columna, dgridRoundRobin)

                                If Columna = dgridRoundRobin.Columns.Count - 1 Then
                                    fila += 1
                                    Columna = 0
                                Else
                                    Columna += 1
                                End If

                            Next


                        ElseIf valresta < 0 Then

                            'EN CASO QUE EL VALOR DEL CPU ES MENOR QUE 0
                            pendientevalor = tblprocesos.Item(3, d).Value

                            'obtiene nombre de color
                            color = tblprocesos.Item(2, d).Value

                            escribir_planificacion(nombreProceso, fila, Columna, dgridRoundRobin)

                            For cant As Integer = 1 To pendientevalor

                                escribir_planificacion(nombreProceso, fila, Columna, dgridRoundRobin)

                                pintar(color, fila, Columna, dgridRoundRobin)

                                If Columna = dgridRoundRobin.Columns.Count - 1 Then
                                    fila += 1
                                    Columna = 0
                                Else
                                    Columna += 1
                                End If

                            Next

                            tblprocesos.Item(3, d).Value = 0

                        End If

                        If d + 1 = No_Procesos Then
                            'QUE RETORNE A LA PRIMERA FILA EN CASO QUE LLEGO AL ULTIMO PROCESO.
                            d = 0
                        Else
                            d += 1
                        End If

                    Else

                        If Contador_Procesos_en_cero = No_Procesos Then
                            estado = 1
                            time.Enabled = False
                        Else

                            Contador_Procesos_en_cero = Procesos_En_Cero()

                            If d + 1 = No_Procesos Then
                                'QUE RETORNE A LA PRIMERA FILA EN CASO QUE LLEGO AL ULTIMO PROCESO.
                                d = 0
                            Else
                                d += 1
                            End If

                        End If

                    End If 'fin if(cpu!=0);


                End While

                Ultima_Fila = fila
                Ultima_Columa = Columna

                dgridRoundRobin.Refresh()
                dgridRoundRobin.ResumeLayout()

            End If


        Catch ex As Exception
            MsgBox(ex.Message)
        End Try

    End Sub
