#revisión 0.2.8 21-07-2023, 00:31 Julia 1.9.2
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
lsf=soil_rankine_forces_ls(grav);#lsf[1,5]=0;#para ignorar el momento resistente de la fuerza pasiva
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

        rfp_n="\$\\phi5/8''\$";#diámetro nominal del refuerzo en la pantalla
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
        while a-compression_zone_depth(As,fy,fc,1)>1e-6
            a=compression_zone_depth(As,fy,fc,1);
            As=long_reinforcement_area(Mu,phim,fy,d,a);
        end

        #elección del área de refuerzo
        Asmin=0.0018*h;
        if As<Asmin As=Asmin end


        dsgn*="\\section{Diseño de refuerzo}";
    end
end

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

\\begin{document}
\\chapter{Reporte de cálculo del muro H=$(hp+hz) m}
\\section{Geometría del muro}

\\begin{figure}[H]
	\\centering
    \\begin{tikzpicture}[scale=2]
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
	\\label{fig:spectre1}
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
