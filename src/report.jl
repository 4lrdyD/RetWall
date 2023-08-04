#revisión 0.4.0 04-08-2023, 00:20 Julia 1.9.2
export report;
function report(mywall::typeIwall;kwargs...)
hp=mywall.hp;
hz=mywall.hz;
t1=mywall.t1;
t2=mywall.t2;
t3=mywall.t3;
b1=mywall.b1;
b2=mywall.b2;
grav=mywall.model;
prop=wall_forces(grav);
rsf=soil_rankine_forces_rs(grav);
lsf=soil_rankine_forces_ls(grav);lsf[1,5]=0;#para ignorar el momento resistente de la fuerza pasiva
uf=uload_rankine_forces_rs(grav,mywall.q,mywall.alpha);
factors=check_stab_wt1(grav,prop,rsf,lsf,arsf=uf);

#---------------------------------------------
#expresión para factor de seguridad por volteo
Mr=sum(prop[:,5]);
Mrs="$(round(Mr,digits=2))"
Mr=sum(rsf[:,6]);
if Mr!=0 Mrs*="+$(round(Mr,digits=2))" end
Mr=sum(lsf[:,5]);
if Mr!=0 Mrs*="+$(round(Mr,digits=2))" end
Mr=sum(uf[:,6]);
if Mr!=0 Mrs*="+$(round(Mr,digits=2))" end
#los momentos actuantes se encuetran en la 5ta columna de rsf y 6ta columna
#de lsf
Ma=sum(rsf[:,5]);
Mas="$(round(Ma,digits=2))";
Ma=sum(lsf[:,6]);
if Ma!=0 Mas*="+$(round(Ma,digits=2))" end
Ma=sum(uf[:,5]);
if Ma!=0 Mas*="+$(round(Ma,digits=2))" end

#-------------------------------------------------------
#expresión para factor de seguridad contra deslizamiento
vf=sum(prop[:,4]);
vfs="($(round(vf,digits=2))";
vf=sum(rsf[:,2]);
if vf!=0 vfs*="+$(round(vf,digits=2))" end
vf=sum(lsf[:,2]);
if vf!=0 vfs*="+$(round(vf,digits=2))" end
vf=sum(uf[:,2]);
if vf!=0 vfs*="+$(round(vf,digits=2))" end
vfs*=")"; vfs1=vfs;
#obteniendo las propiedas de suelo
#obteniendo el último estrato del campo plinels
id=grav.plinels[end,3];
fi=grav.soilprop[id,1];
c=grav.soilprop[id,2];
vfs*="\\tan(\\frac{2}{3}\\times$(round(fi,digits=2))^\\circ)";
B=round(t1+t2+t3+b1+b2,digits=2);
vfs*="+$B\\times\\frac{2}{3}\\times$(round(c,digits=2))";
#el empuje pasivo (resistente), primera columna de lsf
pp=sum(lsf[:,1]);
vfs*="+$(round(pp,digits=2))";
#empuje activo (actuante), primera columna de rsf
pa=sum(rsf[:,1]);
pas="$(round(pa,digits=2))";
pa=sum(uf[:,1]);
if pa!=0 pas*="+$(round(pa,digits=2))" end

#-------------------------------
#texto o mensaje de verificación Ok!! cuando cumple.
fsvs=factors[1]>2 ? ">2\\quad\\textrm{\\textcolor{red}{\\textbf{Ok!!}}}" : "";
fsds=factors[2]>1.5 ? ">1.5\\quad\\textrm{\\textcolor{red}{\\textbf{Ok!!}}}" : "";
es=factors[3]<B/6 ? "<\\dfrac{B}{6}=\\dfrac{$(round(B,digits=2))}{6}=$(round(B/6,digits=3))" : "";
#revisando si se ingreso la capacidad de carga
ncol=size(grav.soilprop)[2];
qa=0;
if ncol>=5
    qa=grav.soilprop[id,5];
 end
qps=factors[4]<qa && factors[5]<qa ?
    "<$(round(qa,digits=2))KN/m^2\\quad\\textrm{\\textcolor{red}{\\textbf{Ok!!}}}" :
    "";

#DISEÑO_________________________________________________________________________
#diseño de refuerzo, se activa mediante la inclusión de la palabra clave design=1
dsgn=""#salida para cuando se requiere diseño de refuerzo
if haskey(kwargs,:design)
    design=kwargs[:design];
    if design==1
        if size(grav.pliners)[1]>1
            err="Por ahora, el diseño solo está preparado para suelos de solo un estrato";
            err*=", si se ingresaron más estratos, solo se usará las propiedades";
            err*=" del primer estrato y por tanto los resultados podrían ser inexactos"
            @warn err
        end
        @inbounds gamma=grav.soilprop[grav.pliners[1,3],3];#peso unitario
        @inbounds Ka=rsf[1,end];#coeficiente de presión activa
        Pap=0.5*gamma*hp^2*Ka;#efecto del suelo de relleno
        Paqp=Ka*hp*mywall.q/cosd(mywall.alpha);#efecto de carga distribuida
        Patp=Pap+Paqp;#Empuje total
        zm=(Pap*hp/3+Paqp*hp/2)/Patp;#punto de aplicación;
        ph=Patp*cosd(mywall.alpha);#componente horizontal
        pv=Patp*sind(mywall.alpha);#componente verticales

        fcv=1.7#factor de carga viva
        if haskey(kwargs,:fcv)#si se ingreso un factor de carga viva diferente
            fcv=kwargs[:fcv];
            if typeof(fcv)<:Real
                if fcv<0 error("fcv debe ser real positivo") end
            else
                error("fcv no es del tipo esperado")
            end
        end
        Mu=fcv*ph*zm;#momento último

        rp=0.04;#recubrimiento en la pantalla
        if haskey(kwargs,:rp)
            rp=kwargs[:rp];
            if typeof(rp)<:Real
                if rp<0 error("rp debe ser real positivo") end
            else
                error("rp no es del tipo esperado")
            end
        end

        rfp_n="\\phi5/8''";#diámetro nominal del refuerzo en la pantalla
        rfp_d=1.588e-2;#diámetro del refuerzo
        rfp_a=2e-4;#área del refuerzo
        if haskey(kwargs,:rlist)
            rfp=kwargs[:rlist];
            if typeof(rfp)==Array{Any,2}
                rfp_n=rfp[1,1];
                rfp_d=rfp[1,2];
                rfp_a=rfp[1,3];
                if typeof(rfp_n)==String && typeof(rfp_d)<:Real && typeof(rfp_a)<:Real
                    if rfp_d<0 || rfp_a<0 error("diámetro y área de refuerzo deben ser positivos") end
                else
                    error("uno o mas componentes de rlist no son del tipo esperado")
                end
            else
                error("rlist no es del tipo esperado")
            end
        end
        h=t1+t2+t3;
        d=h-rp-rfp_d/2;

        phim=0.9;#factor de reducción de resistencia (puede ingresarse como palabra clave)
        if haskey(kwargs,:phim)
            phim=kwargs[:phim];
            if typeof(phim)<:Real
                if phim<0 error("phim debe ser real positivo") end
            else
                error("phim no es del tipo esperado")
            end
        end

        #obteniendo fy y fc, las palabras clave fyid y fcid
        #fyid y fcid son lo indices de material para el acero y el concreto
        #dentro del campo matprop del modelo, dichos materiales den¿berán contener
        #la resistencia a la compresión para el concreto, y el límite de fluencia
        #para el acero.
        fy=420000;fc=21000;#valores por defecto
        if haskey(kwargs,:fyid)
            fyid=kwargs[:fyid];
            if typeof(fyid)==Int64
                if fyid<0 error("fyid debe ser entero positivo") end
                fy=grav.matprop[fyid,4];
            else
                error("fyid no es del tipo esperado")
            end
        end

        if haskey(kwargs,:fcid)
            fcid=kwargs[:fcid];
            if typeof(fcid)==Int64
                if fcid<0 error("fcid debe ser entero positivo") end
                fc=grav.matprop[fcid,1];
            else
                error("fcid no es del tipo esperado")
            end
        end

        #iteración para conseguir el área de refuerzo
        a=d/5;#valor inicial de la profundidad de la zona en compresión
        As=long_reinforcement_area(Mu,phim,fy,d,a);
        while abs(a-compression_zone_depth(As,fy,fc,1))>1e-6
            a=compression_zone_depth(As,fy,fc,1);
            As=long_reinforcement_area(Mu,phim,fy,d,a);
        end

        #elección del área de refuerzo
        Asmin=0.0018*h;
        if As<Asmin As=Asmin end
        #este valor se utilizará para determinar si el dibijo de acero de Refuerzo
        #adicional es necesario, si es necesario el valor será 1
        draw_aditional_path=0;
        if As==Asmin else draw_aditional_path=1; end
        Spf=trunc(rfp_a*100/As)/100;

        dsgn*="\\section{Diseño de refuerzo}
        \\subsection{Pantalla}
        \\begin{align*}
        H_p&=$hp m &\\Rightarrow \\textit{Altura de la pantalla}\\\\
        P_{ap}&=$(round(Pap,digits=2)) KN/m &\\Rightarrow \\textit{efecto del suelo de relleno}\\\\
        P_{aqp}&=$(round(Paqp,digits=2)) KN/m &\\Rightarrow \\textit{efecto de la carga distribuida}\\\\
        P_{atp}&=$(round(Patp,digits=2)) KN/m &\\Rightarrow \\textit{empuje total}\\\\
        \\overline{z}&=$(round(zm,digits=2)) m &\\Rightarrow \\textit{punto de aplicación}\\\\
        P_h&=$(round(ph,digits=2)) KN/m &\\Rightarrow \\textit{Componente horizontal}\\\\
        P_v&=$(round(pv,digits=2)) KN/m &\\Rightarrow \\textit{Componente vertical}\\\\
        M_u&=$(round(Mu,digits=2)) KN.m/m &\\Rightarrow \\textit{Momento último}\\\\
        r&=$(round(rp,digits=3)) m &\\Rightarrow \\textit{Recubrimiento}\\\\
        \\phi_r&=$(rfp_n) &\\Rightarrow \\textit{Diámetro de refuerzo}\\\\
        d&=$(round(d,digits=2)) m &\\Rightarrow \\textit{Peralte efectivo}\\\\
        a&=$(round(a,digits=4)) m &\\Rightarrow \\textit{Profundidad de la zona en compresión}\\\\
        A_s&=$(round(As,digits=6)) m^2 &\\Rightarrow \\textit{área de refuerzo}\\\\
        &$(rfp_n)@$(trunc(rfp_a*100/As)/100) m&\\Rightarrow \\textit{distribución final}\\\\
        \\end{align*}
        ";
        #LONGITUD DE DESARROLLO__________________________________________________
        #se buscará el punto donde el momento sea exactamente igual a la mitad del
        #momento último
        Mus2=Mu/2;
        hm=hp/2#altura inicial desde donde se iniciará la iteración
        Pam=0.5*Ka*gamma*hm^2;#efecto del suelo de relleno
        Paqm=Ka*hm*mywall.q/cosd(mywall.alpha);#efecto de carga distribuida

        while abs(hm-(((Mus2/fcv)-(Paqm*hm/2))/(Pam/(3*hm^2)))^(1/3))>1e-7
            hm=(((Mus2/fcv)-(Paqm*hm/2))/(Pam/(3*hm^2)))^(1/3);
            Pam=0.5*Ka*gamma*hm^2;#efecto del suelo de relleno
            Paqm=Ka*hm*mywall.q/cosd(mywall.alpha);#efecto de carga distribuida
        end
        Patm=Pam+Paqm;#empuje total
        zm=(Pam*hm/3+Paqm*hm/2)/Patm;#punto de aplicación
        ph=Patm*cosd(mywall.alpha);
        pv=Patm*sind(mywall.alpha);
        Mm=Patm*zm*fcv;
        dsgn*="\\subsubsection{Longitud de desarrollo}
        Se buscó por iteración el punto donde el momento sea exactamente la mitad de \$M_u\$\\\\
        \\begin{align*}
        M_u/2&=$(round(Mus2,digits=2)) KN.m/m&\\Rightarrow \\textit{}\\\\
        h_m&=$(round(hm,digits=2)) m&\\Rightarrow \\textit{Medida desde la parte superior de la pantalla}\\\\
        P_{am}&=$(round(Pam,digits=2)) KN/m &\\Rightarrow \\textit{efecto del suelo de relleno}\\\\
        P_{aqm}&=$(round(Paqm,digits=2)) KN/m &\\Rightarrow \\textit{efecto de la carga distribuida}\\\\
        P_{atm}&=$(round(Patm,digits=2)) KN/m &\\Rightarrow \\textit{empuje total}\\\\
        \\overline{z}&=$(round(zm,digits=2)) m &\\Rightarrow \\textit{punto de aplicación}\\\\
        P_h&=$(round(ph,digits=2)) KN/m &\\Rightarrow \\textit{Componente horizontal}\\\\
        P_v&=$(round(pv,digits=2)) KN/m &\\Rightarrow \\textit{Componente vertical}\\\\
        h_c&=$(round(hp-hm,digits=2)) m &\\Rightarrow \\textit{Medida desde la base de la pantalla}\\\\
        L_c&=$(round(hp-hm+d,digits=2)) m &\\Rightarrow h_c+d\\\\
        L_{c_{usar}}&=$(ceil((hp-hm+d)*20)/20) m &\\Rightarrow \\textit{Longitud final}\\\\%elegimos el mayor multiplo de 0.05
        \\end{align*}
        "
        if draw_aditional_path==1
            #calculamos el acero de refuerzo que le correspondería a Mu/2 para determinar
            #si es necesario modificar separaciones
            dm=(t2+t3)*hm/hp+t1-rp-rfp_d/2;
            #iteración para conseguir el área de refuerzo
            a=dm/5;#valor inicial de la profundidad de la zona en compresión
            Asm=long_reinforcement_area(Mu/2,phim,fy,dm,a);
            while abs(a-compression_zone_depth(Asm,fy,fc,1))>1e-6
                a=compression_zone_depth(Asm,fy,fc,1);
                Asm=long_reinforcement_area(Mu/2,phim,fy,dm,a);
            end
            #elección del área de refuerzo
            Asmmin=0.0018*((t2+t3)*hm/hp+t1);
            if Asm<Asmmin Asm=Asmmin end
            Spfm=trunc(rfp_a*100/Asm)/100;
            if Spfm>=2*Spf else Spf=Spfm/2; end
        end

        #VERIFICACIÓN POR CORTE_________________________________________________
        hc=hp-d;#profundidad a la que se verifica por corte
        Pac=0.5*Ka*gamma*hc^2;#efecto del suelo de relleno
        Paqc=Ka*hc*mywall.q/cosd(mywall.alpha);#efecto de la carga distribuida
        Patc=Pac+Paqc;
        ph=Patc*cosd(mywall.alpha);#componente horizontal
        pv=Patc*sind(mywall.alpha);#componente vertical
        Vdu=fcv*ph;

        phic=0.85;#factor de reducción de resistencia (puede ingresarse como palabra clave)
        if haskey(kwargs,:phic)
            phic=kwargs[:phic];
            if typeof(phic)<:Real
                if phic<0 error("phic debe ser real positivo") end
            else
                error("phic no es del tipo esperado")
            end
        end
        Vu=Vdu/phic;#corte último
        Vc=5.25*d*sqrt(fc);#reistencia al corte
        Vce=Vc*2/3;#resistencia al corte reducida
        check=Vce>Vu ? "\\quad\\textrm{\\textcolor{red}{\\textbf{Ok!!}}}" : "";
        dsgn*="\\subsubsection{Verificación por corte}
        \\begin{align*}
        h_c&=$(round(hc,digits=2)) m&\\Rightarrow \\textit{profundidad a la que se verifica el corte }h_p-d\\\\
        P_{ac}&=$(round(Pac,digits=2)) KN/m&\\Rightarrow \\textit{efecto del suelo de relleno}\\\\
        h_{aqc}&=$(round(Paqc,digits=2)) KN/m&\\Rightarrow \\textit{efecto de la carga distribuida}\\\\
        P_{atc}&=$(round(Patc,digits=2)) KN/m &\\Rightarrow \\textit{empuje total}\\\\
        P_h&=$(round(ph,digits=2)) KN/m &\\Rightarrow \\textit{Componente horizontal}\\\\
        P_v&=$(round(pv,digits=2)) KN/m &\\Rightarrow \\textit{Componente vertical}\\\\
        V_{du}&=$(round(Vdu,digits=2)) KN/m &\\Rightarrow \\textit{Corte mayorado}\\\\
        V_{u}&=$(round(Vu,digits=2)) KN/m &\\Rightarrow \\textit{Corte último } V_{du}/\\phi\\\\
        V_{c}&=$(round(Vc,digits=2)) KN/m &\\Rightarrow \\textit{Resistencia de la sección } 5.25bd\\sqrt{f'c}\\\\
        V_{ce}&=$(round(Vce,digits=2)) KN/m $check &\\Rightarrow \\textit{Resistencia reducida}\\\\
        \\end{align*}
        "
        #ACERO DE TEMPERATURA___________________________________________________
        #Arriba
        Astu=0.002*(t1+(t2+t3)/3);
        Astm=0.002*(t1+2*(t2+t3)/3);
        Astd=0.002*(t1+t2+t3);
        rfpf_n="\\phi1/2''";#diámetro nominal del acero de temperatura frontal
        rfpf_d=1.27e-2;#diámetro del refuerzo
        rfpf_a=1.29e-4;#área del refuerzo
        rfpp_n="\\phi3/8''";#diámetro nominal del acero de temperatura posterior
        rfpp_d=0.953e-2;#diámetro del refuerzo
        rfpp_a=0.71e-4;#área del refuerzo
        if haskey(kwargs,:rlist)
            rfp=kwargs[:rlist];
            if typeof(rfp)==Array{Any,2}
                rfpf_n=rfp[3,1];
                rfpf_d=rfp[3,2];
                rfpf_a=rfp[3,3];
                rfpp_n=rfp[4,1];
                rfpp_d=rfp[4,2];
                rfpp_a=rfp[4,3];
                if typeof(rfpf_n)==String && typeof(rfpf_d)<:Real && typeof(rfpf_a)<:Real
                    if rfpf_d<0 || rfpf_a<0 error("diámetro y área de refuerzo deben ser positivos") end
                else
                    error("uno o mas componentes de rlist no son del tipo esperado")
                end

                if typeof(rfpp_n)==String && typeof(rfpp_d)<:Real && typeof(rfpp_a)<:Real
                    if rfpp_d<0 || rfpp_a<0 error("diámetro y área de refuerzo deben ser positivos") end
                else
                    error("uno o mas componentes de rlist no son del tipo esperado")
                end
            else
                error("rlist no es del tipo esperado")
            end
        end
        dsgn*="\\subsubsection{Acero horizontal-temperatura}
        \\begin{table}[H]
        \\parbox{.3\\linewidth}{
        \\centering
        \\begin{tabular}{llc}
        \\textbf{Arriba}&&\\\\
        \$A_{st}\$&$(round(Astu,digits=6)) m\$^2\$&\\\\
        \$2/3A_{st}\$&$(round(Astu*2/3,digits=6)) m\$^2\$&\\\\
        \$\\phi_r\$&\$$rfpf_n\$&\\\\
        S&$(trunc(rfpf_a/(Astu*2/3)*100)/100) m&\\\\
        &&\\\\
        \$1/3A_{st}\$&$(round(Astu*1/3,digits=6)) m\$^2\$&\\\\
        \$\\phi_r\$&\$$rfpp_n\$&\\\\
        S&$(trunc(rfpp_a/(Astu*1/3)*100)/100) m&\\\\
        \\end{tabular}
        }
        \\hfill
        \\parbox{.3\\linewidth}{
        \\centering
        \\begin{tabular}{llc}
        \\textbf{Intermedio}&&\\\\
        \$A_{st}\$&$(round(Astm,digits=6)) m\$^2\$&\\\\
        \$2/3A_{st}\$&$(round(Astm*2/3,digits=6)) m\$^2\$&\\\\
        \$\\phi_r\$&\$$rfpf_n\$&\\\\
        S&$(trunc(rfpf_a/(Astm*2/3)*100)/100) m&\\\\
        &&\\\\
        \$1/3A_{st}\$&$(round(Astm*1/3,digits=6)) m\$^2\$&\\\\
        \$\\phi_r\$&\$$rfpp_n\$&\\\\
        S&$(trunc(rfpp_a/(Astm*1/3)*100)/100) m&\\\\
        \\end{tabular}
        }
        \\hfill
        \\parbox{.3\\linewidth}{
        \\centering
        \\begin{tabular}{llc}
        \\textbf{Abajo}&&\\\\
        \$A_{st}\$&$(round(Astd,digits=6)) m\$^2\$&\\\\
        \$2/3A_{st}\$&$(round(Astd*2/3,digits=6)) m\$^2\$&\\\\
        \$\\phi_r\$&\$$rfpf_n\$&\\\\
        S&$(trunc(rfpf_a/(Astd*2/3)*100)/100) m&\\\\
        &&\\\\
        \$1/3A_{st}\$&$(round(Astd*1/3,digits=6)) m\$^2\$&\\\\
        \$\\phi_r\$&\$$rfpp_n\$&\\\\
        S&$(trunc(rfpp_a/(Astd*1/3)*100)/100) m&\\\\
        \\end{tabular}
        }
        \\end{table}
        "
        #DISEÑO DE LA ZAPATA___________________________________________________
        Ws=gamma*hp;#presión del terreno sobre la zapata
        gammac=24;#peso específico del Concreto
        if haskey(kwargs,:fcid)
            fcid=kwargs[:fcid];
            if typeof(fcid)==Int64
                if fcid<0 error("fcid debe ser entero positivo") end
                gammac=grav.matprop[fcid,3];
            else
                error("fcid no es del tipo esperado")
            end
        end
        Wpp=gammac*hz;#presión por peso propio
        #Zapata frontal.........................................................
        Wumax=fcv*factors[4]-0.9*Wpp;#factors[4]: presión en el pie de la zapata
        Muzf=0.5*Wumax*b1^2;

        rz=0.075;#recubrimiento en la zapata
        if haskey(kwargs,:rz)
            rz=kwargs[:rz];
            if typeof(rz)<:Real
                if rz<0 error("rz debe ser real positivo") end
            else
                error("rz no es del tipo esperado")
            end
        end

        rfpz_n="\\phi5/8''";#diámetro nominal del refuerzo en la zapata
        rfpz_d=1.588e-2;#diámetro del refuerzo
        rfpz_a=2e-4;#área del refuerzo
        if haskey(kwargs,:rlist)
            rfp=kwargs[:rlist];
            if typeof(rfp)==Array{Any,2}
                rfpz_n=rfp[2,1];
                rfpz_d=rfp[2,2];
                rfpz_a=rfp[2,3];
                if typeof(rfpz_n)==String && typeof(rfpz_d)<:Real && typeof(rfpz_a)<:Real
                    if rfpz_d<0 || rfpz_a<0 error("diámetro y área de refuerzo deben ser positivos") end
                else
                    error("uno o mas componentes de rlist no son del tipo esperado")
                end
            else
                error("rlist no es del tipo esperado")
            end
        end

        dz=hz-rz-rfpz_d/2;
        #iteración para conseguir el área de refuerzo
        az=dz/5;#valor inicial de la profundidad de la zona en compresión
        Aszf=long_reinforcement_area(Muzf,phim,fy,dz,az);
        while abs(az-compression_zone_depth(Aszf,fy,fc,1))>1e-6
            az=compression_zone_depth(Aszf,fy,fc,1);
            Aszf=long_reinforcement_area(Muzf,phim,fy,dz,az);
        end

        #elección del área de refuerzo
        Aszfmin=0.0018*hz;
        if Aszf<Aszfmin Aszf=Aszfmin end

        dsgn*="\\subsection{Zapata}
        \\begin{tikzpicture}[scale=2]%dibujo ilustrativo
        \\draw (0,0)--++(-90:1)--++(180:0.9)--++(-90:0.5)--++(0:3.7)--++(90:0.5)
        --++(180:2.3)--(0.4,0);
        \\draw (-0.1,0)--(0.1,0)--++(90:0.2)--(0.3,-0.2)--(0.3,0)--(0.5,0);
        \\draw (-0.9,-1.7)--(2.8,-1.7)--(2.8,-2)--(-0.9,-2.5)--cycle;
        \\draw [dashed] (2.8,-2)--(-0.9,-2);
        \\foreach \\x in {0,0.1,0.2,...,1}
        {
        \\draw [->](0.5+\\x*2.3,-0.7)--(0.5+\\x*2.3,-1);
        }
        \\foreach \\x in {0,0.1,0.2,...,1}
        {
        \\draw [->](0.5+\\x*2.3,-1.15)--(0.5+\\x*2.3,-1.35);
        }
        \\draw (2,-0.7) node[above]{\$W_s\$};
        \\draw (0.5,-1.25) node[left]{\$W_{pp}\$};
        \\draw (2.8,-2) node[right]{\$q_{\\text{talón}}\$};
        \\draw (-0.9,-2.5) node[left]{\$q_{\\text{pie}}\$};
        \\draw [->](0.5,-2.3108)--(0.5,-2);
        \\draw [->](1,-2.2432)--(1,-2);
        \\draw (0.5,-2.15) node[left]{\$q_b\$};
        \\draw (1,-2.12) node[left]{\$q_d\$};
        \\Cote[0.1cm] {(0.5,-1.7)}{(1,-1.7)}{\\tiny{\$d\$}}[
            Cote node/.append style={below}];
        \\end{tikzpicture}
        \\begin{align*}
        W_s&=$(round(Ws,digits=2)) KN/m/m&\\Rightarrow \\textit{Presión del terreno sobre la zapata}\\\\
        W_{pp}&=$(round(Wpp,digits=2)) KN/m/m&\\Rightarrow \\textit{presión por peso propio}\\\\
        \\end{align*}
        \\subsubsection{Zapata frontal}
        \\begin{align*}
        W_{\\text{umáx}}&=$(round(Wumax,digits=2)) KN/m/m&\\Rightarrow \\textit{Presión última}\\\\
        M_{u}&=$(round(Muzf,digits=2)) KN.m/m&\\Rightarrow \\textit{Momento último}\\\\
        \\phi_r&=$rfpz_n &\\Rightarrow \\textit{Diámetro de refuerzo}\\\\
        r&=$(round(rz,digits=3)) m &\\Rightarrow \\textit{Recubrimiento}\\\\
        d&=$(round(dz,digits=2)) m &\\Rightarrow \\textit{Peralte efectivo}\\\\
        a&=$(round(az,digits=4)) m &\\Rightarrow \\textit{Profundidad de la zona en compresión}\\\\
        A_s&=$(round(Aszf,digits=6)) m^2 &\\Rightarrow \\textit{Área de refuerzo}\\\\
        &$(rfpz_n)@$(trunc(rfpz_a*100/Aszf)/100) m&\\Rightarrow \\textit{distribución final}\\\\
        \\end{align*}
        "
        #Zapata posterior.......................................................
        fcm=1.4;#factor de carga muerta
        if haskey(kwargs,:fcm)#si se ingreso un factor de carga muerta diferente
            fcm=kwargs[:fcm];
            if typeof(fcm)<:Real
                if fcm<0 error("fcm debe ser real positivo") end
            else
                error("fcm no es del tipo esperado")
            end
        end
        qb=fcm*(factors[4]-factors[5])*b2/(b1+t2+t1+t3+b2);
        Wur=fcm*(Ws+Wpp-factors[5]);
        Muzp=Wur*b2^2/2-qb*b2^2/6;#momento último
        az=dz/5;#valor inicial de la profundidad de la zona en compresión
        Aszp=long_reinforcement_area(Muzp,phim,fy,dz,az);
        while abs(az-compression_zone_depth(Aszp,fy,fc,1))>1e-6
            az=compression_zone_depth(Aszp,fy,fc,1);
            Aszp=long_reinforcement_area(Muzp,phim,fy,dz,az);
        end
        #elección del área de refuerzo
        Aszpmin=0.0018*hz;
        if Aszp<Aszpmin Aszp=Aszpmin end

        dsgn*="\\subsubsection{Zapata posterior}
        \\begin{align*}
        q_b&=$(round(qb,digits=2)) KN/m/m&\\Rightarrow \\textit{Ver figura de arriba}\\\\
        W_{ur}&=$(round(Wur,digits=2)) KN/m/m&\\Rightarrow W_{ur}=fcm(W_s+W_{pp}-q_{\\text{talón}})\\\\
        M_{u}&=$(round(Muzp,digits=2)) KN.m/m&\\Rightarrow \\textit{Momento último}\\\\
        \\phi_r&=$rfpz_n &\\Rightarrow \\textit{Diámetro de refuerzo}\\\\
        r&=$(round(rz,digits=3)) m &\\Rightarrow \\textit{Recubrimiento}\\\\
        d&=$(round(dz,digits=2)) m &\\Rightarrow \\textit{Peralte efectivo}\\\\
        a&=$(round(az,digits=4)) m &\\Rightarrow \\textit{Profundidad de la zona en compresión}\\\\
        A_s&=$(round(Aszp,digits=6)) m^2 &\\Rightarrow \\textit{Área de refuerzo}\\\\
        &$(rfpz_n)@$(trunc(rfpz_a*100/Aszp)/100) m&\\Rightarrow \\textit{distribución final}\\\\
        \\end{align*}
        "
        #Verificación a cortante
        qd=fcm*(factors[4]-factors[5])*(b2-dz)/(b1+t2+t1+t3+b2);
        Vdu=fcm*(Ws+Wpp-factors[5])*(b2-dz)-0.5*qd*(b2-dz);
        Vn=Vdu/phic;
        Vc=5.25*dz*sqrt(fc);#reistencia al corte
        check=Vc>Vn ? "\\quad\\textrm{\\textcolor{red}{\\textbf{Ok!!}}}" : "";

        dsgn*="\\subsubsection{Verificación por corte}
        \\begin{align*}
        q_d&=$(round(qd,digits=2)) m&\\Rightarrow \\textit{Ver figura de arriba}\\\\
        V_{du}&=$(round(Vdu,digits=2)) KN/m &\\Rightarrow \\textit{Corte mayorado}\\\\
        V_{u}&=$(round(Vn,digits=2)) KN/m &\\Rightarrow \\textit{Corte último } V_{du}/\\phi\\\\
        V_{c}&=$(round(Vc,digits=2)) KN/m $check&\\Rightarrow \\textit{Resistencia de la sección } 5.25bd\\sqrt{f'c}\\\\
        \\end{align*}
        "
        #Refuerzo transversal-temperatura
        dsgn*="\\subsubsection{Refuerzo transversal-temperatura}
        \\begin{align*}
        A_{s-temp}&=$(round(0.0018*hz,digits=6)) m^2&\\Rightarrow \\textit{Refuerzo transversal}\\\\
        \\phi_r &=$rfpz_n&\\Rightarrow \\textit{Diámetro de refuerzo}\\\\
        S &=$(trunc(rfpz_a/(0.0018*hz)*100)/100) m&\\Rightarrow \\textit{Separación}\\\\
        \\end{align*}
        "
        #Dibujo
        #lineas longitudinales
        #coordenadas de los nodos en la pantalla posterior
        xpi=b1+t2+t1+t3; ypi=hz;
        xps=b1+t2+t1; yps=hz+hp;
        dist=hypot(xps-xpi,yps-ypi);
        Cs=(xps-xpi)/dist;
        Sn=(yps-ypi)/dist;
        #obteniendo coordenadas para el acero de refuerzo principal en la cara
        #posterior
        rp_path=VolatileArray(zeros(Float64,0),0,0);
        rp_path[1,1]=xps-rp/Sn-rp*Cs/Sn-0.15;
        rp_path[1,2]=yps-rp;
        rp_path[2,1]=xps-rp/Sn-rp*Cs/Sn;
        rp_path[2,2]=yps-rp;
        rp_path[3,1]=xpi-rp/Sn-(hz/Sn-rz/Sn)*Cs;
        rp_path[3,2]=ypi-hz+rz;
        rp_path[4,1]=xpi-rp/Sn-(hz/Sn-rz/Sn)*Cs+0.4;
        rp_path[4,2]=ypi-hz+rz;
        #obteniendo coordenadas para el acero de refuerzo adicional en la cara posterior
        #de la pantalla
        if draw_aditional_path==1
            rpa_path=VolatileArray(zeros(Float64,0),0,0);
            rpa_path[1,1]=rp_path[2,1]-.03;
            rpa_path[1,2]=rp_path[2,2];
            rpa_path[2,1]=rp_path[3,1]-.03;
            rpa_path[2,2]=rp_path[3,2];
            rpa_path[3,1]=rp_path[4,1]-.03;
            rpa_path[3,2]=rp_path[4,2];
            dist=hypot(rpa_path[1,1]-rpa_path[2,1],rpa_path[1,2]-rpa_path[2,2]);
            Cs=(rpa_path[1,1]-rpa_path[2,1])/dist;
            Sn=(rpa_path[1,2]-rpa_path[2,2])/dist;
            rpa_path[1,1]=rpa_path[2,1]+(hz-rz+ceil((hp-hm+d)*20)/20)*Cs/Sn;
            rpa_path[1,2]=rpa_path[2,2]+hz-rz+ceil((hp-hm+d)*20)/20;
            dap=draw_polyline_lcode(Array(rpa_path),1,2,3,ops="line width=0.2mm");
            #temperatura pantalla posterior
            rptd_path=VolatileArray(zeros(Float64,0),0,0);#parte inferior
            S=trunc(rfpp_a/(Astd*1/3)*100)/100
            rptd_path[1,1]=rp_path[3,1]+(hz-rz)*Cs/Sn-rfpp_d/(2*Sn);
            rptd_path[1,2]=rp_path[3,2]+hz-rz
            rptd_path[2,1]=rptd_path[1,1]+hp*Cs/(3*Sn);
            rptd_path[2,2]=rptd_path[1,2]+hp/3;
            rptm_path=VolatileArray(zeros(Float64,0),0,0);#parte media
            rptm_path[1,1]=rptd_path[1,1]+ceil((hp/(3*Sn))/S)*S*Cs;
            rptm_path[1,2]=rptd_path[1,2]+ceil((hp/(3*Sn))/S)*S*Sn;
            rptm_path[2,1]=rptd_path[1,1]+hp*Cs*2/(3*Sn);
            rptm_path[2,2]=rptd_path[1,2]+hp*2/3;
            S=trunc(rfpp_a/(Astm*1/3)*100)/100
            rptu_path=VolatileArray(zeros(Float64,0),0,0);#parte superior
            dist=hypot(rptm_path[1,1]-rptm_path[2,1],rptm_path[1,2]-rptm_path[2,2]);
            rptu_path[1,1]=rptm_path[1,1]+ceil(dist/S)*S*Cs;
            rptu_path[1,2]=rptm_path[1,2]+ceil(dist/S)*S*Sn;
            rptu_path[2,1]=rp_path[2,1]-rfpp_d/(2*Sn);
            rptu_path[2,2]=rp_path[2,2];
            #corrigiendo la ubicación del acero transversal en el dibujo en
            #uno de los tramos por refuerzo adicional
            Lc=ceil((hp-hm+d)*20)/20;
            L1=rptd_path[2,2];L2=rptm_path[1,2];L3=rptm_path[2,2];L4=rptu_path[1,2];L5=rptu_path[2,2];#límietes clave
            if L1>hz+Lc
                S=trunc(rfpp_a/(Astd*1/3)*100)/100
                rptd_path[1,1]=rptd_path[1,1]-.03;
                help_path=VolatileArray(zeros(Float64,0),0,0);#camino de ayuda
                help_path[2,1]=rptd_path[2,1];
                help_path[2,2]=rptd_path[2,2];
                rptd_path[2,1]=rptd_path[1,1]+(hz+Lc)*Cs/Sn;
                rptd_path[2,2]=rptd_path[1,2]+hz+Lc;
                dist=hypot(rptd_path[1,1]-rptd_path[2,1],rptd_path[2,1]-rptd_path[2,2]);
                help_path[1,1]=rptd_path[1,1]+ceil(dist/S)*S*Cs+.03;
                help_path[1,2]=rptd_path[1,2]+ceil(dist/S)*S*Sn;
                if help_path[2,2]>=help_path[1,2]
                    draw_help=draw_along_path_lcode(help_path,rfpp_d/2,S);
                end
            elseif L1<=hz+Lc<=L2
                rptd_path[1,1]=rptd_path[1,1]-.03;
                rptd_path[2,1]=rptd_path[2,1]-.03;
                draw_help="";
            elseif L2<hz+Lc<L3
                rptd_path[1,1]=rptd_path[1,1]-.03;
                rptd_path[2,1]=rptd_path[2,1]-.03;
                S=trunc(rfpp_a/(Astm*1/3)*100)/100
                rptm_path[1,1]=rptm_path[1,1]-.03;
                help_path=VolatileArray(zeros(Float64,0),0,0);#camino de ayuda
                help_path[2,1]=rptm_path[2,1];
                help_path[2,2]=rptm_path[2,2];
                rptm_path[2,1]=rptd_path[1,1]+(hz+Lc)*Cs/Sn;
                rptm_path[2,2]=rptd_path[1,2]+hz+Lc;
                dist=hypot(rptm_path[1,1]-rptm_path[2,1],rptm_path[2,1]-rptm_path[2,2]);
                help_path[1,1]=rptm_path[1,1]+ceil(dist/S)*S*Cs+.03;
                help_path[1,2]=rptm_path[1,2]+ceil(dist/S)*S*Sn;
                if help_path[2,2]>=help_path[1,2]
                    draw_help=draw_along_path_lcode(help_path,rfpp_d/2,S);
                end
            elseif L3<=hz+Lc<=L4
                rptd_path[1,1]=rptd_path[1,1]-.03;
                rptd_path[2,1]=rptd_path[2,1]-.03;
                rptm_path[1,1]=rptm_path[1,1]-.03;
                rptm_path[2,1]=rptm_path[2,1]-.03;
                draw_help="";
            elseif L4<hz+Lc<L5
                rptd_path[1,1]=rptd_path[1,1]-.03;
                rptd_path[2,1]=rptd_path[2,1]-.03;
                rptm_path[1,1]=rptm_path[1,1]-.03;
                rptm_path[2,1]=rptm_path[2,1]-.03;
                S=trunc(rfpp_a/(Astu*1/3)*100)/100
                rptu_path[1,1]=rptu_path[1,1]-.03;
                help_path=VolatileArray(zeros(Float64,0),0,0);#camino de ayuda
                help_path[2,1]=rptu_path[2,1];
                help_path[2,2]=rptu_path[2,2];
                rptu_path[2,1]=rptd_path[1,1]+(hz+Lc)*Cs/Sn;
                rptu_path[2,2]=rptd_path[1,2]+hz+Lc;
                dist=hypot(rptu_path[1,1]-rptu_path[2,1],rptu_path[2,1]-rptu_path[2,2]);
                help_path[1,1]=rptu_path[1,1]+ceil(dist/S)*S*Cs+.03;
                help_path[1,2]=rptu_path[1,2]+ceil(dist/S)*S*Sn;
                if help_path[2,2]>=help_path[1,2]
                    draw_help=draw_along_path_lcode(help_path,rfpp_d/2,S);
                end
            end
        else
            draw_help="";
            dap="";
            #temperatura pantalla posterior
            rptd_path=VolatileArray(zeros(Float64,0),0,0);#parte inferior
            S=trunc(rfpp_a/(Astd*1/3)*100)/100
            rptd_path[1,1]=rp_path[3,1]+(hz-rz)*Cs/Sn-rfpp_d/(2*Sn);
            rptd_path[1,2]=rp_path[3,2]+hz-rz
            rptd_path[2,1]=rptd_path[1,1]+hp*Cs/(3*Sn);
            rptd_path[2,2]=rptd_path[1,2]+hp/3;
            rptm_path=VolatileArray(zeros(Float64,0),0,0);#parte media
            rptm_path[1,1]=rptd_path[1,1]+ceil((hp/(3*Sn))/S)*S*Cs;
            rptm_path[1,2]=rptd_path[1,2]+ceil((hp/(3*Sn))/S)*S*Sn;
            rptm_path[2,1]=rptd_path[1,1]+hp*Cs*2/(3*Sn);
            rptm_path[2,2]=rptd_path[1,2]+hp*2/3;
            S=trunc(rfpp_a/(Astm*1/3)*100)/100
            rptu_path=VolatileArray(zeros(Float64,0),0,0);#parte superior
            dist=hypot(rptm_path[1,1]-rptm_path[2,1],rptm_path[1,2]-rptm_path[2,2]);
            rptu_path[1,1]=rptm_path[1,1]+ceil(dist/S)*S*Cs;
            rptu_path[1,2]=rptm_path[1,2]+ceil(dist/S)*S*Sn;
            rptu_path[2,1]=rp_path[2,1]-rfpp_d/(2*Sn);
            rptu_path[2,2]=rp_path[2,2];
        end

        #coordenadas de los nodos en la pantalla frontal
        xpi=b1; ypi=hz;
        xps=b1+t2; yps=hz+hp;
        dist=hypot(xps-xpi,yps-ypi);
        Cs=(xps-xpi)/dist;
        Sn=(yps-ypi)/dist;
        #obteniendo coordenadas para el acero de refuerzo principal en la cara
        #frontal
        rf_path=VolatileArray(zeros(Float64,0),0,0);
        rf_path[1,1]=xps+rp/Sn-rp*Cs/Sn+0.15;
        rf_path[1,2]=yps-rp;
        rf_path[2,1]=xps+rp/Sn-rp*Cs/Sn;
        rf_path[2,2]=yps-rp;
        rf_path[3,1]=xpi+rp/Sn-(hz/Sn-rz/Sn)*Cs;
        rf_path[3,2]=ypi-hz+rz;
        rf_path[4,1]=xpi+rp/Sn-(hz/Sn-rz/Sn)*Cs+0.4;
        rf_path[4,2]=ypi-hz+rz;
        #temperatura pantalla frontal
        rftd_path=VolatileArray(zeros(Float64,0),0,0);#parte inferior
        S=trunc(rfpf_a/(Astd*2/3)*100)/100
        rftd_path[1,1]=rf_path[3,1]+(hz-rz)*Cs/Sn+rfpf_d/(2*Sn);
        rftd_path[1,2]=rf_path[3,2]+hz-rz
        rftd_path[2,1]=rftd_path[1,1]+hp*Cs/(3*Sn);
        rftd_path[2,2]=rftd_path[1,2]+hp/3;
        rftm_path=VolatileArray(zeros(Float64,0),0,0);#parte media
        rftm_path[1,1]=rftd_path[1,1]+ceil((hp/(3*Sn))/S)*S*Cs;
        rftm_path[1,2]=rftd_path[1,2]+ceil((hp/(3*Sn))/S)*S*Sn;
        rftm_path[2,1]=rftd_path[1,1]+hp*Cs*2/(3*Sn);
        rftm_path[2,2]=rftd_path[1,2]+hp*2/3;
        S=trunc(rfpf_a/(Astm*2/3)*100)/100
        rftu_path=VolatileArray(zeros(Float64,0),0,0);#parte superior
        dist=hypot(rftm_path[1,1]-rftm_path[2,1],rftm_path[1,2]-rftm_path[2,2]);
        rftu_path[1,1]=rftm_path[1,1]+ceil(dist/S)*S*Cs;
        rftu_path[1,2]=rftm_path[1,2]+ceil(dist/S)*S*Sn;
        rftu_path[2,1]=rf_path[2,1]+rfpf_d/(2*Sn);
        rftu_path[2,2]=rf_path[2,2];

        #coordenadas de los nodos en cara inferior de la zapata
        xpi=0; ypi=0;
        xps=b1+t2+t1+t3+b2; yps=0;
        #obteniendo coordenadas para el acero de refuerzo en la cara
        #inferior de la zapata
        rzi_path=VolatileArray(zeros(Float64,0),0,0);
        rzi_path[1,1]=rz;
        rzi_path[1,2]=rz+.15;
        rzi_path[2,1]=rz;
        rzi_path[2,2]=rz;
        rzi_path[3,1]=xps-rz;
        rzi_path[3,2]=rz;
        rzi_path[4,1]=xps-rz;
        rzi_path[4,2]=rz+.15;
        #para refuerzo transversal
        S=trunc(rfpz_a/(0.0018*hz)*100)/100
        rzit_path=VolatileArray(zeros(Float64,0),0,0);#para refuerzo transversal
        rzit_path[1,1]=rz+rfpz_d/2;
        rzit_path[1,2]=rz+rfpz_d/2;
        rzit_path[2,1]=xps-rz-rfpz_d/2;
        rzit_path[2,2]=rz+rfpz_d/2;
        dist=hypot(rzit_path[1,1]-rzit_path[2,1],rzit_path[1,2]-rzit_path[2,2]);
        nb=trunc(dist/S);
        rzit_path[1,1]+=(dist-nb*S)/2;

        #coordenadas de los nodos en cara superior de la zapata
        xpi=0; ypi=hz;
        xps=b1+t2+t1+t3+b2; yps=hz;
        #obteniendo coordenadas para el acero de refuerzo en la cara superior
        #de la zapata
        rzs_path=VolatileArray(zeros(Float64,0),0,0);
        rzs_path[1,1]=rz;
        rzs_path[1,2]=ypi-rz-.15;
        rzs_path[2,1]=rz;
        rzs_path[2,2]=ypi-rz;
        rzs_path[3,1]=xps-rz;
        rzs_path[3,2]=ypi-rz;
        rzs_path[4,1]=xps-rz;
        rzs_path[4,2]=ypi-rz-.15;
        #para refuerzo transversal
        S=trunc(rfpz_a/(0.0018*hz)*100)/100
        rzst_path=VolatileArray(zeros(Float64,0),0,0);#para refuerzo transversal
        rzst_path[1,1]=rz+rfpz_d/2;
        rzst_path[1,2]=hz-rz-rfpz_d/2;
        rzst_path[2,1]=xps-rz-rfpz_d/2;
        rzst_path[2,2]=hz-rz-rfpz_d/2;
        dist=hypot(rzst_path[1,1]-rzst_path[2,1],rzst_path[1,2]-rzst_path[2,2]);
        nb=trunc(dist/S);
        rzst_path[1,1]+=(dist-nb*S)/2;

        #Obteniendo la escala para el dibujo de muro con refuerzo
        esc=15/(b1+t2+t1+t3+b2+2.5);

        dsgn*="\\begin{figure}[H]
        	\\centering
            \\begin{tikzpicture}[scale=$esc]
                $(draw_polyline_lcode(Array(grav.nod),1,2,3,8,10,9,5,4,close=1))
                $(draw_soil_surface_lcode(mywall,1))
                $(draw_wall_dimensions_lcode(mywall))
                $draw_help
                $(draw_along_path_lcode(rzst_path,rfpz_d/2,trunc(rfpz_a/(0.0018*hz)*100)/100))
                $(draw_along_path_lcode(rzit_path,rfpz_d/2,trunc(rfpz_a/(0.0018*hz)*100)/100))
                $(draw_along_path_lcode(rftd_path,rfpf_d/2,trunc(rfpf_a/(Astd*2/3)*100)/100))
                $(draw_along_path_lcode(rftm_path,rfpf_d/2,trunc(rfpf_a/(Astm*2/3)*100)/100))
                $(draw_along_path_lcode(rftu_path,rfpf_d/2,trunc(rfpf_a/(Astu*2/3)*100)/100))
                $(draw_along_path_lcode(rptd_path,rfpp_d/2,trunc(rfpp_a/(Astd*1/3)*100)/100))
                $(draw_along_path_lcode(rptm_path,rfpp_d/2,trunc(rfpp_a/(Astm*1/3)*100)/100))
                $(draw_along_path_lcode(rptu_path,rfpp_d/2,trunc(rfpp_a/(Astu*1/3)*100)/100))
                $(draw_polyline_lcode(Array(rp_path),1,2,3,4,ops="line width=0.2mm"))
                $(draw_polyline_lcode(Array(rf_path),1,2,3,4,ops="line width=0.2mm"))
                $(draw_polyline_lcode(Array(rzi_path),1,2,3,4,ops="line width=0.2mm"))
                $(draw_polyline_lcode(Array(rzs_path),1,2,3,4,ops="line width=0.2mm"))
                $dap
                $(draw_leader_lcode((1.4,0.5),pos="below right",label="\$\\phi5/8''@0.15m\$",type="circle",diameter=0.04*esc))
            \\end{tikzpicture}
          \\caption{Distribución de refuerzo}
        	\\label{fig:distr}
        \\end{figure}"
    end
end
#escala para la geometría del muro
esc=15/(b1+t2+t1+t3+b2+3);
a="
\\documentclass[oneside,spanish]{scrbook}
\\usepackage[spanish, es-nodecimaldot, es-tabla]{babel}
\\usepackage{float}
\\usepackage{siunitx}
\\sisetup{
  round-mode          = places,
  round-precision     = 2,
  detect-all=true
}
\\usepackage{amsmath}
\\usepackage{tikz}
\\usetikzlibrary{babel,calc}
\\usetikzlibrary{arrows.meta}
\\usepackage{xparse}
\\usepackage{pgfplots}
\\pgfplotsset{compat=newest}
\\usepgfplotslibrary{units}
% Para poder acotar elementos
%ver:
%https://tex.stackexchange.com/a/180110/203837
%https://tex.stackexchange.com/a/298345/203837
\\usepackage{ifluatex}
\\ifluatex
\\usepackage{pdftexcmds}
\\makeatletter
\\let\\pdfstrcmp\\pdf@strcmp
\\let\\pdffilemoddate\\pdf@filemoddate
\\makeatother
\\fi
\\tikzset{%
    Cote node/.style={
        midway,
        fill=white,
        inner sep=1.5pt,
        outer sep=2pt
    },
    Cote arrow/.style={
        <->,
        >=latex,
        very thin
    }
}

\\makeatletter
\\NewDocumentCommand{\\Cote}{
    s       % acotación con flechas afuera
    D<>{1.5pt} % desplazamiento de línea
    O{.75cm}    % desplazamiento de acotación
    m       % primer punto
    m       % segundo punto
    m       % etiqueta
    D<>{o}  % () coordenadas -> ángulo
            % h -> horizontal,
            % v -> vertical
            % o lo que sea -> oblicuo
    O{}     % parámetro tikzset
    }{

    {\\tikzset{#8}

    \\coordinate (@1) at #4 ;
    \\coordinate (@2) at #5 ;

    \\if #7H % acotar línea horizontal
        \\coordinate (@0) at (\$(\$#4!.5!#5\$) + (#3,0)\$) ;
        \\coordinate (@5) at (\$#5+(#3,0)\$) ;
        \\coordinate (@4) at (\$#4+(#3,0)\$) ;
    \\else
    \\if #7V % acotar línea vertical
        \\coordinate (@0) at (\$(\$#4!.5!#5\$) + (#3,0)\$) ;
        \\coordinate (@5) at (\$#5+(0,#3)\$) ;
        \\coordinate (@4) at (\$#4+(0,#3)\$) ;
    \\else
    \\if #7v % acotación vertical
        \\coordinate (@0) at (\$(\$#4!.5!#5\$) + (#3,0)\$) ;
        \\coordinate (@4) at (@0|-@1) ;
        \\coordinate (@5) at (@0|-@2) ;
    \\else
    \\if #7h % acotación horizontal
        \\coordinate (@0) at (\$(\$#4!.5!#5\$) + (0,#3)\$) ;
        \\coordinate (@4) at (@0-|@1) ;
        \\coordinate (@5) at (@0-|@2) ;
    \\else % acotación concava
    \\ifnum\\pdfstrcmp{\\unexpanded\\expandafter{\\@car#7\\@nil}}{(}=\\z@
        \\coordinate (@5) at (\$#7!#3!#5\$) ;
        \\coordinate (@4) at (\$#7!#3!#4\$) ;
    \\else % acotación oblicua
        \\coordinate (@5) at (\$#5!#3!90:#4\$) ;
        \\coordinate (@4) at (\$#4!#3!-90:#5\$) ;
    \\fi\\fi\\fi\\fi\\fi

    \\draw[very thin,shorten >= #2,shorten <= -2*#2] (@4) -- #4 ;
    \\draw[very thin,shorten >= #2,shorten <= -2*#2] (@5) -- #5 ;

    \\IfBooleanTF #1 {% con estrella
    \\draw[Cote arrow,-] (@4) -- (@5)
        node[Cote node] {#6\\strut};
    \\draw[Cote arrow,<-] (@4) -- (\$(@4)!-6pt!(@5)\$) ;
    \\draw[Cote arrow,<-] (@5) -- (\$(@5)!-6pt!(@4)\$) ;
    }{% sin estrella
    \\ifnum\\pdfstrcmp{\\unexpanded\\expandafter{\\@car#7\\@nil}}{(}=\\z@
        \\draw[Cote arrow] (@5) to[bend right]
            node[Cote node] {#6\\strut} (@4) ;
    \\else
    \\draw[Cote arrow] (@4) -- (@5)
        node[Cote node] {#6\\strut};
    \\fi
    }}
    }

\\makeatother
\\setcounter{secnumdepth}{3}
\\begin{document}
\\chapter{Reporte de cálculo del muro H=$(hp+hz) m}
\\section{Geometría del muro}

\\begin{figure}[H]
	\\centering
    \\begin{tikzpicture}[scale=$esc]
        $(draw_polyline_lcode(Array(grav.nod),1,2,3,8,10,9,5,4,close=1))
        $(draw_polyline_lcode(Array(grav.nod),5,8,ops="dashed"))
        $(draw_polyline_lcode(Array(grav.nod),7,10,ops="dashed"))
        $(draw_elm_label_lcode(prop))
        $(draw_soilp_rs_lcode(grav,-1,1))
        $(draw_soilp_ls_lcode(grav,maximum(grav.nod[:,1])+1,-0.5-mywall.D/2))
        $(draw_soil_surface_lcode(mywall,1))
        $(draw_spliners_lcode(grav))
        $(draw_wall_dimensions_lcode(mywall))
        $(draw_qload_lcode(mywall,1))
    \\end{tikzpicture}
  \\caption{Geometría del muro de contención}
	\\label{fig:geom}
\\end{figure}
\\section{Cálculo}
Para calcular la coeficiente de presión activa de Rankine usamos:\\\\
\\begin{multline}
$(ka_rankine_equation_lcode(c=1))
\\end{multline}

Para suelos granulares (\$c'=0\$), esta formula se reduce a:\\\\
\\begin{equation}
$(ka_rankine_equation_lcode())
\\end{equation}\\\\

Reemplazando los parámetros correspondientes obtenemos:
\\begin{table}[H]
\\caption{Coeficientes de presión activa y fuerzas del terreno}
\\label{tab:rsf}
\\centering
\\resizebox{\\linewidth}{!}{
\\begin{tabular}{|m{1.5cm}|m{1.5cm}|m{1.7cm}|m{1.7cm}|m{1.5cm}|m{1.5cm}|m{1.5cm}|m{1.5cm}|}
$(print_rsf_lcode(rsf))
\\end{tabular}}
\\end{table}
\\ifdim 0.0 pt=$(round(mywall.q,digits=0)) pt
\\else
    Por su parte, las fuerzas debidas a la carga distribuida son:
    \\begin{table}[H]
    \\caption{Fuerzas debidas a la carga distribuida \$q=$(mywall.q)KN/m^2\$}
    \\label{tab:uf}
    \\centering
    \\begin{tabular}{|m{1.5cm}|m{1.7cm}|m{1.7cm}|m{1.5cm}|m{1.5cm}|m{1.5cm}|m{1.5cm}|}
    $(print_uf_lcode(uf))
    \\end{tabular}
    \\end{table}
\\fi
\$F_xb_y\$ es el momento actuante principal y \$F_yb_x\$ contribuye a la
resistencia. Las fuerzas generadas por el peso del muro se muestran en la
siguiente tabla:
\\begin{table}[H]
\\caption{Fuerzas generadas por el muro}
\\label{tab:wforce}
\\centering
\\begin{tabular}{|m{1.5cm}|m{1.5cm}|m{2cm}|m{1.5cm}|m{2.5cm}|}
$(print_wf_lcode(prop))
\\end{tabular}
\\end{table}

Para calcular el coeficiente de presión pasiva de Rankine usamos:\\\\
\\begin{multline}
$(kp_rankine_equation_lcode(c=1))
\\end{multline}

Para suelos granulares (\$c'=0\$), esta formula se reduce a:\\\\
\\begin{equation}
$(kp_rankine_equation_lcode())
\\end{equation}\\\\

Reemplazando los parámetros correspondientes obtenemos:
\\begin{table}[H]
\\caption{Coeficientes de presión pasiva y fuerzas del terreno}
\\label{tab:rsf}
\\centering
\\resizebox{\\linewidth}{!}{
\\begin{tabular}{|m{1.5cm}|m{1.5cm}|m{1.7cm}|m{1.7cm}|m{1.5cm}|m{1.5cm}|m{1.5cm}|m{1.5cm}|}
$(print_lsf_lcode(lsf))
\\end{tabular}}
\\end{table}
\$F_x\$ y el momento que genera (\$F_xb_y\$) contribuyen a la resistencia por
deslizamiento y por volteo respectivamente, \$F_yb_x\$ es un momento actuante.\\\\

Factor de seguridad contra el volteo:\\\\
\\begin{align*}
FS_{volteo}=\\dfrac{\\Sigma M_R}{M_o}=\\dfrac{$Mrs}{$Mas}=
    $(round(factors[1],digits=2))$fsvs\\\\
\\end{align*}

Factor de seguridad contra el deslizamiento:\\\\
\\begin{align*}
FS_{deslizamiento}&=$(slip_factor_equation_lcode())\\\\
&=\\dfrac{$(vfs)}{$(pas)}\\\\
&=$(round(factors[2],digits=2))$fsds
\\end{align*}
Revisión por falla por capacidad de carga:\\\\
\\begin{align*}
e&=$(eccentricity_equation_lcode())\\\\
&=\\dfrac{$(round(B,digits=2))}{2}-\\dfrac{($Mrs)-($Mas)}{$vfs1}\\\\
&=$(round(factors[3],digits=3)) m$es\\\\
\\end{align*}
\\begin{align*}
q_{tal\\acute on}^{pie}&=$(soil_pressure_equation_lcode())\\\\
&=\\dfrac{$vfs1}{$(round(B,digits=2))}\\left(1\\pm\\dfrac
    {6\\times$(round(factors[3],digits=3))}{$(round(B,digits=2))}\\right)\\\\
q_{pie}&=$(round(factors[4],digits=2))KN/m^2\\\\
q_{tal\\acute on}&=$(round(factors[5],digits=2))KN/m^2$qps
\\end{align*}
$(dsgn)
\\end{document}
"
open("prueba1.tex", "w") do f
           write(f, a)
           end
#run(pipeline(`pdflatex prueba1`,stdout="log.txt",stderr="err.txt"));
run(`pdflatex prueba1`);
    if Sys.iswindows()
        run(`cmd /c start prueba1.pdf`);
    elseif Sys.islinux()
        run(`xdg-open prueba1.pdf`)
    else
    end
end

"""
    draw_wall_lcode(model::Wmodel{<:Real})
Retorna el código Latex para dibujar los elementos del muro (campo `elm`)
,el código deberá ser insertado dentro de un entorno `tickpicture` en Latex.
"""
function draw_wall_lcode(model::Wmodel{<:Real})
    #debe ser insertado dentro de un entorno tikzpicture
    out="";
    #número de elementos
    nel=size(model.elm)[1];

    joints=Array(model.nod);

    #pbreak indica el punto donde terminan los elementos
    #triangulares
    if model.pbreak>0
        for i in 1:model.pbreak
            id1=model.elm[i,1];
            id2=model.elm[i,2];
            id3=model.elm[i,3];
            out*=draw_polyline_lcode(joints,id1,id2,id3,close=1);
        end
    end

    if model.pbreak<nel
        for i in model.pbreak+1:nel
            id1=model.elm[i,1];
            id2=model.elm[i,2];
            id3=model.elm[i,3];
            id4=model.elm[i,4];
            out*=draw_polyline_lcode(joints,id1,id2,id3,id4,close=1);
        end
    end
    return out;
end

"""
    draw_elm_label_lcode(prop::VolatileArray{<:Real,2})
Retorna el código Latex para dibujar etiquetas ubicadas en los centroides de
los elementos que conforman el muro, el argumento (`prop`), debería ser
generado usando la función `wall_forces`, donde las coordenads `x` e `y` de cada
elemento están en la columna 2 y 3 respectivamente, el código deberá ser
insertado dentro de un entorno `tickpicture` en Latex.
"""
function draw_elm_label_lcode(prop::VolatileArray{<:Real,2})
    #debe ser insertado dentro de un entorno tikzpicture
    out="";
    for i in 1:size(prop)[1]
        out*="\\draw ($(prop[i,2]),$(prop[i,3]))node{\\small{$i}};
        \\draw ($(prop[i,2]),$(prop[i,3]))circle(0.075cm);
        "
    end
    return out;
end

"""
    draw_polyline_lcode(joints::Array{<:Real,2},args::Int64...;
        ops::String="",close::Int64=0)
Retorna el código Latex para dibujar una polilínea através de los puntos
ingresados.
    *`joints`: matriz de puntos, en cada fila, una coordenada `[x y]`.
    *`args...`: id's o ubicación de los puntos en `joints` que determinan la
    polilínea.
    *`ops`: palabra clave opcional, son las opciones de dibujo en Latex.
    *`close`: palabra clave opcional, `close=1` para cerrar la polilínea luego
    de alcanzar el último punto.
El código deberá ser insertado dentro de un entorno `tickpicture` en Latex.
"""
function draw_polyline_lcode(joints::Array{<:Real,2},args::Int64...;
    ops::String="",close::Int64=0)
    #debe ser insertado dentro de un entorno tikzpicture
    out="
        \\draw"*"["*ops*"]";
    out*="
        ($(joints[args[1],1]),$(joints[args[1],2]))";
    nel=length(args);
    for i in 2:nel
        id=args[i];
        out*="--
        ($(joints[id,1]),$(joints[id,2]))"
    end
    out*=(close==0 ? ";" : "--cycle;");
    return out;
end

function ka_rankine_equation_lcode(;c::Int64=0)
    out="";
    if c==0
        out="K_a=\\cos\\alpha\\frac{\\cos\\alpha-\\sqrt{\\cos^2\\alpha-
        \\cos^2\\phi'}}{\\cos\\alpha+\\sqrt{\\cos^2\\alpha-
        \\cos^2\\phi'}}";
    else
        out="\\frac{K_a}{\\cos\\alpha}=\\frac{1}{\\cos^2\\phi'}
        \\Bigg(2\\cos^2\\alpha+2\\frac{c'}{\\gamma{z}}\\cos\\phi'\\sen\\phi'\\\\
        -\\sqrt{4\\cos^2\\alpha(\\cos^2\\alpha-\\cos^2\\phi')+
        4\\left(\\frac{c'}{\\gamma{z}}\\right)^2\\cos^2\\phi'+8\\frac{c'}{\\gamma{z}}
        \\cos^2\\alpha\\sen\\phi'\\cos\\phi'}\\Bigg)-1";
    end
    return out;
end

function kp_rankine_equation_lcode(;c::Int64=0)
    out="";
    if c==0
        out="K_p=\\cos\\alpha\\frac{\\cos\\alpha+\\sqrt{\\cos^2\\alpha-
        \\cos^2\\phi'}}{\\cos\\alpha-\\sqrt{\\cos^2\\alpha-
        \\cos^2\\phi'}}";
    else
        out="\\frac{K_p}{\\cos\\alpha}=\\frac{1}{\\cos^2\\phi'}
        \\Bigg(2\\cos^2\\alpha+2\\frac{c'}{\\gamma{z}}\\cos\\phi'\\sen\\phi'\\\\
        +\\sqrt{4\\cos^2\\alpha(\\cos^2\\alpha-\\cos^2\\phi')+
        4\\left(\\frac{c'}{\\gamma{z}}\\right)^2\\cos^2\\phi'+8\\frac{c'}{\\gamma{z}}
        \\cos^2\\alpha\\sen\\phi'\\cos\\phi'}\\Bigg)-1";
    end
    return out;
end

kr_maku_equation_lcode()="K_o=(1-\\sen\\phi')OCR^{\\sen\\phi'}";
kr_sch_equation_lcode()="K_o=0.5\\left(OCR\\right)^{0.5}";
slip_factor_equation_lcode()="\\dfrac{\\Sigma V\\tan{(k_1\\phi')}+Bk_2c'_2+P_p}
    {P_a\\cos\\alpha}";
eccentricity_equation_lcode()="\\dfrac{B}{2}-\\dfrac{\\Sigma M_R-\\Sigma M_O}
    {\\Sigma V}";
soil_pressure_equation_lcode()="\\dfrac{\\Sigma V}{B}\\left(1\\pm
    \\dfrac{6e}{B}\\right)";

function draw_soilp_rs_lcode(model::Wmodel{<:Real},offs::Real...)
    out="";
    pline=model.pliners;
    nel=size(pline)[1];
    noffs=length(offs);
    aoffs=1;#para controlar que no se supere el número de desplazamientos
            #ingresados
    nsp=size(model.soilprop)[1];
    if nsp==0
        error("No se ha ingresado ninguna propiedad de suelo");
    end
    for i in 1:nel
        p1=pline[i,1];
        p2=pline[i,2];
        ids=pline[i,3];
        #propiedades del suelo
        if ids>nsp
            error("Propiedad de suelo (id=$ids) no encontrada");
        end
        fi=model.soilprop[ids,1];
        c=model.soilprop[ids,2];
        gamma=model.soilprop[ids,3];
        if c<1e-6
            c="0"
        else
            c="$(c)KPa"
        end
        #generando punto de inserción
        dp=(model.pnod[p1,:]+model.pnod[p2,:])/2;
        #aplicando desplazamientos
        if aoffs<=noffs
            dp[1]+=offs[aoffs];
            aoffs+=1;
        end
        if aoffs<=noffs
            dp[2]+=offs[aoffs];
            aoffs+=1;
        end
        out*="\\draw ($(dp[1]),$(dp[2]))node[align=left]{
            \\small{\$\\phi'=$(fi)^\\circ\$}\\\\
            \\small{\$c'=$(c)\$}\\\\
            \\small{\$\\gamma=$(gamma)KN/m^3\$}};
        "
    end
    return out;
end

function draw_soilp_ls_lcode(model::Wmodel{<:Real},offs::Real...)
    out="";
    pline=model.plinels;
    nel=size(pline)[1];
    noffs=length(offs);
    aoffs=1;#para controlar que no se supere el número de desplazamientos
            #ingresados
    nsp=size(model.soilprop)[1];
    if nsp==0
        error("No se ha ingresado ninguna propiedad de suelo");
    end

    for i in 1:nel
        p1=pline[i,1];
        p2=pline[i,2];
        ids=pline[i,3];
        #propiedades del suelo
        if ids>nsp
            error("Propiedad de suelo (id=$ids) no encontrada");
        end
        fi=model.soilprop[ids,1];
        c=model.soilprop[ids,2];
        gamma=model.soilprop[ids,3];
        if c<1e-6
            c="0"
        else
            c="$(c)KPa"
        end
        #generando punto de inserción
        dp=(model.pnod[p1,:]+model.pnod[p2,:])/2;
        #aplicando desplazamientos
        if aoffs<=noffs
            dp[1]+=offs[aoffs];
            aoffs+=1;
        end
        if aoffs<=noffs
            dp[2]+=offs[aoffs];
            aoffs+=1;
        end
        out*="\\draw ($(dp[1]),$(dp[2]))node[align=left]{
            \\small{\$\\phi'=$(fi)^\\circ\$}\\\\
            \\small{\$c'=$(c)\$}\\\\
            \\small{\$\\gamma=$(gamma)KN/m^3\$}};
        "
    end
    return out;
end

function draw_soil_surface_lcode(wall::typeIwall,offs::Real=0)
    model=wall.model;
    out="";
    #linea de la superficie derecha
    #obteniendo coordenadas del nudo superior derecho
    #el nudo en la fila 10 en model.nod es el nudo superior derecho del muro
    nod=model.nod;
    maxy=nod[10,2];
    maxx=nod[10,1];
    #obteniendo vector con componente horizontal unitaria en la dirección de la
    #pendiente del terreno
    alpha=deg2rad(wall.alpha);
    uy=tan(alpha);#ux=1, implícito
    #maximo valor de x, en la matriz de nudos
    B=maximum(model.nod[:,1]);
    #desplazando
    B+=offs;
    uy*=(wall.t3+wall.b2+offs);
    out*="\\draw ($(maxx),$(maxy))--($B,$(maxy+uy));
        "
    #línea de la superficie izquierda
    #los nudos 5 y 9 forman la pared frontal del muro
    x1=model.nod[5,1];
    y1=model.nod[5,2];
    x2=model.nod[9,1];
    y2=model.nod[9,2];
    if (abs(x2-x1)>1e-6)
        #pendiente de la recta 5->9
        m=(y2-y1)/(x2-x1);
        x2=x1+(model.D-y1)/m;
    end
    out*="\\draw (-.5,$(wall.D))--($(x2),$(wall.D));"
end

function draw_spliners_lcode(model::Wmodel{<:Real})
    pline=model.pliners;
    npl=size(pline)[1];
    if npl==0
        error("No se ha construído la línea de presiones")
    end
    p1=pline[1,1];
    p2=pline[end,2];
    x1=model.pnod[p1,1];
    y1=model.pnod[p1,2];
    x2=model.pnod[p2,1];
    y2=model.pnod[p2,2];
    out="\\draw[dashed] ($(x1),$(y1))--($(x2),$(y2));
        ";
    pline=model.plinels;
    p1=pline[1,1];
    p2=pline[end,2];
    x1=model.pnod[p1,1];
    y1=model.pnod[p1,2];
    x2=model.pnod[p2,1];
    y2=model.pnod[p2,2];
    out*="\\draw[dashed] ($(x1),$(y1))--($(x2),$(y2));
    ";
    return out;
end

function draw_wall_dimensions_lcode(wall::typeIwall)
    model=wall.model;
    nod=model.nod;
    #horizontal
    out="";
    x1=0; x2=nod[5,1]; dim=round(x2-x1,digits=2);
    if (dim>0)
    out*="\\Cote[0.35cm] {($(x1),0)}{($(x2),0)}{\\small{$dim m}}[
        Cote node/.append style={below}];
        ";
    end
    x1=x2; x2=nod[8,1]; dim=round(x2-x1,digits=2);
    if (dim>0)
    out*="\\Cote[0.35cm] {($(x1),0)}{($(x2),0)}{\\small{$dim m}}[
        Cote node/.append style={below}];
        ";
    end
    x1=x2; x2=nod[3,1]; dim=round(x2-x1,digits=2);
    if (dim>0)
    out*="\\Cote[0.35cm] {($(x1),0)}{($(x2),0)}{\\small{$dim m}}[
        Cote node/.append style={below}];
        ";
    end
    #vertical hz, hp
    x1=nod[2,1]; y2=nod[3,2]; dim=round(y2,digits=2);
    if (dim>0)
    out*="\\Cote[0.35cm]{($(x1),0)}{($(x2),$(y2))}{\\small{$dim m}}[
        Cote node/.append style={right}];
        ";
    end
    y1=y2; y2=nod[10,2]; dim=round(y2-y1,digits=2);
    if (dim>0)
    out*="\\Cote[0.35cm]{($(x1),$(y1))}{($(x2),$(y2))}{\\small{$dim m}}[
        Cote node/.append style={right}];
        ";
    end
    #t1
    x1=nod[9,1]; y1=nod[9,2]; x2=nod[10,1]; y2=y1; dim=round(x2-x1,digits=2);
    if (dim>0)
    out*="\\Cote[-0.35cm]{($(x1),$(y1))}{($(x2),$(y2))}{\\small{$dim m}}[
        Cote node/.append style={above}];
        ";
    end
    #t2 y t3 solo sin ambos son >0
    dim=round(nod[6,1]-nod[5,1],digits=2);
    dim1=round(nod[8,1]-nod[7,1],digits=2);
    if dim>0 && dim1>0
        x1=nod[5,1];
        y1=nod[5,2];
        x2=nod[6,1];
        y2=nod[6,2];
        out*="\\Cote[0.35cm]{($(x1),$(y1))}{($(x2),$(y2))}{\\small{$dim m}}[
            Cote node/.append style={above}];
            ";
        x1=nod[7,1];
        y1=nod[7,2];
        x2=nod[8,1];
        y2=nod[8,2];
        out*="\\Cote[0.35cm]{($(x1),$(y1))}{($(x2),$(y2))}{\\small{$dim1 m}}[
                Cote node/.append style={above}];
                ";
    end
    #D
    y2=wall.D; dim=round(y2,digits=2);
    if (dim>0)
    out*="\\Cote[-0.35cm]{(0,0)}{(0,$y2)}{\\small{$dim m}}[
        Cote node/.append style={left}];
        ";
    end
    #alfa
    dim=wall.alpha;
    alpha=deg2rad(dim);
    y3=nod[10,2]+(wall.t3+wall.b2)*tan(alpha);
    x3=nod[3,1];
    x1=x3+.5;
    y1=y3;
    x2=x3+.5*cos(alpha);
    y2=y3+.5*sin(alpha);
    dim=round(dim,digits=2);
    if dim>0
        out*="\\Cote{($x2,$y2)}{($x1,$y1)}{\\small{$dim\$^\\circ\$}}
            <($x3,$y3)>[Cote node/.append style={right=.5cm}];
            \\draw[dashed] ($x3,$y3)--($(x3+1),$y3);
            ";
    end
    return out;
end

function draw_qload_lcode(wall::typeIwall,offs::Real=0)
    model=wall.model;
    nod=model.nod;
    q=wall.q;
    out="%carga distribuida
    ";
    if q>1e-6
        alpha=deg2rad(wall.alpha);
        y2=nod[10,2];
        x2=nod[10,1];
        x1=x2
        y1=y2+.5;
        maxx=x2+wall.t3+wall.b2+offs;
        while x2<maxx
            out*="\\draw[->] ($x1,$y1)--($x2,$y2);
            ";
            y2+=.5*tan(alpha);
            x2+=.5;
            x1=x2
            y1=y2+.5;
        end
        #etiqueta
        x1=maxx-(wall.t3+wall.b2+offs)/2;
        y1=nod[10,2]+(x1-nod[10,1])*tan(alpha)+.75;
        out*="\\draw ($x1,$y1)node{\\small{\$q=$(q)KN/m^2\$}};
        "
    end
    return out;
end

"""
    function print_table_lcode(rsf::VolatileArray{<:Real,2},ids::Int64...;
        header::Array{String,2}=Array{String,2}(undef,0,0),kwargs...)
Genera código latex para imprimir una tabla:

    *`rsf`: es una matriz a partir del cuál se generará la tabla.

    *`ids`: es una secuencia de ids que indicará el orden en el cuál se
    imprimirán las columnas de `rsf` si no se ingresa ningún id, el orden será
    el que tiene `rsf`.

    *`header`: array de Strings que contendrá las cabeceras de la tabla, debe
    tener el mismo tamaño que el conjunto de `ids` ingresado, o si no se
    ingresaron ids el mismo número de columnas de `rsf`.

    *`kwargs`: secuencia de opciones como palabras clave, para generar la tabla
    `precision1=p1`,`precision2=p2`,..., `precisionn=pn` son las precisiones
    correspondientes para el redondeo de la columna `n`.
El String generado debe insertarse dentro de un entorno tabular en códido latex.
"""
function print_table_lcode(rsf::VolatileArray{<:Real,2},ids::Int64...;
    header::Array{String,2}=Array{String,2}(undef,0,0),kwargs...)
    out="\\hline
    ";
    dy=size(rsf)[1];
    dx=size(rsf)[2];
    ncols=length(ids);
    if ncols==0
        ids=1:dx;
    end
    if length(header)==0
    else
        if length(header)==length(ids)
            cont=1;
            for i in ids
                out*="\\textbf{\$$(header[cont])\$}"
                i==ids[end] ? out*="\\\\" : out*="&";
                cont+=1;
            end
            out*="
            \\hline";
        else
            error("La cabecera y número de columnas a usar no son compatibles")
        end
    end
    for i in 1:dy
        for j in ids
            #buscando si se ingresaron precisiones para la columna actual
            #creando variable local que guardará la precisión (entero)
            local prs::Int64;
            prsh="precision$j"
            prsh=Meta.parse(prsh);
            haskey(kwargs,prsh) ? prs=kwargs[prsh] : prs=2;
            if prs!=0
                out*="\$$(round(rsf[i,j],digits=prs))\$"
            else
                out*="\$$(round(Int64,rsf[i,j]))\$"
            end
            j==ids[end] ? out*="\\\\" : out*="&";
        end
        out*="
        \\hline";
    end
    return out;
end

function print_rsf_lcode(rsf::VolatileArray{T,2}) where {T<:Real}
    #imprime la tabla generadas por el terreno
    #debe insertarse dentro de un entorno tabular en latex
    nel=size(rsf)[1];
    strat=collect(T,1:nel);
    #agregando una columna (id de estrato)
    insert!(rsf,1,strat,dim=2);
    header=["Estrato" "K_a" "F_x(KN/m)" "F_y(KN/m)" "b_x(m)" "b_y(m)" "F_xb_y" "F_yb_x"];
    out=print_table_lcode(rsf,1,8,2,3,4,5,6,7,header=header,precision1=0,
        precision8=4);
    #eliminando la columna agregada.
    deleteat!(rsf,1,dim=2);
    return out;
end

function print_lsf_lcode(lsf::VolatileArray{T,2}) where {T<:Real}
    #imprime la tabla generadas por el terreno (pasivo)
    #debe insertarse dentro de un entorno tabular en latex
    nel=size(lsf)[1];
    strat=collect(T,1:nel);
    #agregando una columna (id de estrato)
    insert!(lsf,1,strat,dim=2);
    header=["Estrato" "K_p" "F_x(KN/m)" "F_y(KN/m)" "b_x(m)" "b_y(m)" "F_xb_y" "F_yb_x"];
    out=print_table_lcode(lsf,1,8,2,3,4,5,6,7,header=header,precision1=0,
        precision8=4);
    #eliminando la columna agregada.
    deleteat!(lsf,1,dim=2);
    return out;
end

function print_uf_lcode(uf::VolatileArray{T,2}) where {T<:Real}
    #imprime la tabla generadas por el terreno
    #debe insertarse dentro de un entorno tabular en latex
    nel=size(uf)[1];
    strat=collect(T,1:nel);
    #agregando una columna (id de estrato)
    insert!(uf,1,strat,dim=2);
    header=["Estrato" "F_x(KN/m)" "F_y(KN/m)" "b_x(m)" "b_y(m)" "F_xb_y" "F_yb_x"];
    out=print_table_lcode(uf,1,2,3,4,5,6,7,header=header,precision1=0);
    #eliminando la columna agregada.
    deleteat!(uf,1,dim=2);
    return out;
end

function print_wf_lcode(wforce::VolatileArray{T,2}) where {T<:Real}
    #imprime la tabla de fuerzas generadas por el muro
    #debe insertarse dentro de un entorno tabular en latex
    nel=size(wforce)[1];
    strat=collect(T,1:nel);
    #agregando una columna (id de estrato)
    insert!(wforce,1,strat,dim=2);
    header=["Num" "A(m^2)" "P(KN/m)" "b(m)" "M(KN.m/m)"];
    out=print_table_lcode(wforce,1,2,5,3,6,header=header,precision1=0);
    #eliminando la columna agregada.
    deleteat!(wforce,1,dim=2);
    return out;
end

function draw_along_path_lcode(path::VolatileArray{T,2},r::Real,d::Real) where {T<:Real}
    #dibuja circulos de radio r, a lo largo de un camino determinado por los
    #puntos en path, estos estarán separados una distancia d
    #debe insertarse dentro de un entorno tikzpicture
    if size(path)[2]<2 error("Se esperaba una matriz de al menos dos columnas") end
    if size(path)[1]<2 error("Se esperaba una matriz de al menos dos filas") end
    out="";
    np=size(path)[1];#número de puntos por recorrer
    rd=0;#distancia residual, se actualizará en cada tramo
    da=0;#distancia acumulada
    Ca=0;#coseno del ángulo
    Sa=0;#seno del ángulo
    out*="\\draw($(path[1,1]),$(path[1,2])) circle($r);
    "
    for i in 1:np-1
        x1=path[i,1];y1=path[i,2];#obteniendo las coordenadas de dos puntos
        x2=path[i+1,1];y2=path[i+1,2];#consecutivos
        dist=hypot(x2-x1,y2-y1);
        if dist==0 Ca=0;Sa=0; else Ca=(x2-x1)/dist;Sa=(y2-y1)/dist; end
        xi=x1+Ca*(d-rd);yi=y1+Sa*(d-rd);
        if hypot(xi-x1,yi-y1)<=dist
            lim=da+rd+dist;
            out*="\\draw($xi,$yi) circle($r);
            ";
            da+=d;
            while da+d<=lim
                xi+=Ca*d;yi+=Sa*d;
                out*="\\draw($xi,$yi) circle($r);
                ";
                da+=d;
            end
            rd=lim-da;
        else
            rd+=dist;
        end
    end
    return out;
end


function draw_leader_lcode(p::Tuple{T,N};kwargs...) where {T<:Real, N<:Real}
    #dibujará un leader para indicar algún elementos, las palabrás clave son
    #pos::String, que puede ser right, left, above, below, o la combinación de estos
    #label::String, La etiqueta del indicador
    #type::String, tipo de indicador, arrow o circle
    #diameter<:Real, diametro del circulo, suncionará solo si se elige el tipo circle
    out="";
    x=p[1];
    y=p[2];
    pos="above right";#posición por defecto
    label="";#etiqueta por defecto
    type="arrow";#tipo por defecto
    diameter=0.04;#diametro por defecto

    if haskey(kwargs,:label)
        label=kwargs[:label];
        if typeof(label)!=String
            error("label no es del tipo esperado")
        end
    end

    draw_ops="[<-]";
    if haskey(kwargs,:type)
        type=kwargs[:type];
        if typeof(type)!=String
            error("type no es del tipo esperado")
        end

        if type=="circle"
            if haskey(kwargs,:diameter)
                diameter=kwargs[:diameter];
                if typeof(diameter)<:Real
                    if diameter<=0 error("diameter debe ser real positivo") end
                else
                    error("diameter no es del tipo esperado")
                end
            end
            draw_ops="[{Circle[open,width=$(diameter)cm,length=$(diameter)cm]}-, shorten <= -$(diameter/2)cm]"
        elseif type=="arrow"
        else
            error("no se esperaba type=$type")
        end
    end


    if haskey(kwargs,:pos)
        pos=kwargs[:pos];
        if typeof(pos)!=String
            error("pos no es del tipo esperado")
        end
    end

    if pos=="right"
        out*="\\draw $draw_ops ($x,$y)--++(0:0.25) node[right]{\\tiny{$label}};"
    elseif pos=="left"
        out*="\\draw $draw_ops ($x,$y)--++(0:-0.25) node[left]{\\tiny{$label}};"
    elseif pos=="above"
        out*="\\draw $draw_ops ($x,$y)--++(90:0.25) node[above]{\\tiny{$label}};"
    elseif pos=="below"
        out*="\\draw $draw_ops ($x,$y)--++(90:-0.25) node[below]{\\tiny{$label}};"
    elseif pos=="above right" || pos=="right above"
        out*="\\draw $draw_ops ($x,$y)--++(45:0.25)--++(0:0.25) node[right]{\\tiny{$label}};"
    elseif pos=="below right" || pos=="right below"
        out*="\\draw $draw_ops ($x,$y)--++(-45:0.25)--++(0:0.25) node[right]{\\tiny{$label}};"
    elseif pos=="below left" || pos=="left below"
        out*="\\draw $draw_ops ($x,$y)--++(-135:0.25)--++(0:-0.25) node[left]{\\tiny{$label}};"
    elseif pos=="above left" || pos=="left above"
        out*="\\draw $draw_ops ($x,$y)--++(135:0.25)--++(0:-0.25) node[right]{\\tiny{$label}};"
    end

    return out;
end
