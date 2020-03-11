#revisión 0.1.0 11-03-2020, 00:35 Julia1.1.0
function report(mywall::typeIwall)
hp=mywall.hp;
hz=mywall.hz;
t1=mywall.t1;
t2=mywall.t2;
t3=mywall.t3;
b1=mywall.b1;
b2=mywall.b2;
grav=mywall.model;
prop=wall_forces(grav);
a="
\\documentclass[oneside,spanish]{scrbook}
\\usepackage[spanish, es-nodecimaldot, es-tabla]{babel}
\\usepackage{amsmath}
\\usepackage{tikz}
\\usetikzlibrary{calc}
\\usepackage{xparse}
\\usepackage{pgfplots}
\\pgfplotsset{compat=newest}
\\usepgfplotslibrary{units}
\\usepackage{ifluatex}
\\ifluatex
% Para poder acotar elementos
%ver:
%https://tex.stackexchange.com/a/180110/203837
%https://tex.stackexchange.com/a/298345/203837
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
\\chapter{Geometría del muro}
Las dimensiones del muro son:\\\\
\\begin{align*}
    hp &= $hp m\\\\
    hz &= $hz m\\\\
    t1 &= $t1 m\\\\
    t2 &= $t2 m\\\\
    t3 &= $t3 m\\\\
    b1 &= $b1 m\\\\
    b2 &= $b2 m\\\\
\\end{align*}
\\begin{figure}
	\\centering
    \\begin{tikzpicture}[scale=2.5]
        $(draw_polyline_lcode(Array(grav.nod),1,2,3,8,10,9,5,4,close=1))
        $(draw_polyline_lcode(Array(grav.nod),5,8,ops="dashed"))
        $(draw_polyline_lcode(Array(grav.nod),7,10,ops="dashed"))
        $(draw_elm_label_lcode(prop))
        $(draw_soilp_rs_lcode(grav,1))
        $(draw_soilp_ls_lcode(grav,maximum(grav.nod[:,1])+1,-0.5-grav.D/2))
        $(draw_soil_surface_lcode(grav))
        $(draw_spliners_lcode(grav))
    \\end{tikzpicture}
  \\caption{Geometría del muro de contención}
	\\label{fig:spectre1}
\\end{figure}

$(ka_rankine_equation_lcode(c=1))
$(kr_sch_equation_lcode())

\\end{document}
"
open("prueba1.tex", "w") do f
           write(f, a)
           end
#run(pipeline(`pdflatex prueba1`,stdout="log.txt",stderr="err.txt"));
run(`pdflatex prueba1`);
run(`cmd /c start prueba1.pdf`);
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

function ka_rankine_equation_lcode(;c::Int64=0,wn::Int64=0)
    head="*"
    if wn!=0
        head=""
    end
    out="";
    if c==0
        out="\\begin{equation$head}
        K_a=\\cos\\alpha\\frac{\\cos\\alpha-\\sqrt{\\cos^2\\alpha-
        \\cos^2\\phi'}}{\\cos\\alpha+\\sqrt{\\cos^2\\alpha-
        \\cos^2\\phi'}}
        \\end{equation$head}";
    else
        out="\\begin{multline$head}
        \\frac{K_a}{\\cos\\alpha}=\\frac{1}{\\cos^2\\phi'}
        \\Bigg(2\\cos^2\\alpha+2\\frac{c'}{\\gamma{z}}\\cos\\phi'\\sen\\phi'\\\\
        -\\sqrt{4\\cos^2\\alpha(\\cos^2\\alpha-\\cos^2\\phi')+
        4\\left(\\frac{c'}{\\gamma{z}}\\right)^2\\cos^2\\phi'+8\\frac{c'}{\\gamma{z}}
        \\cos^2\\alpha\\sen\\phi'\\cos\\phi'}\\Bigg)-1
        \\end{multline$head}";
    end
    return out;
end

function kp_rankine_equation_lcode(;c::Int64=0,wn::Int64=0)
    head="*"
    if wn!=0
        head=""
    end
    out="";
    if c==0
        out="\\begin{equation$head}
        K_p=\\cos\\alpha\\frac{\\cos\\alpha+\\sqrt{\\cos^2\\alpha-
        \\cos^2\\phi'}}{\\cos\\alpha-\\sqrt{\\cos^2\\alpha-
        \\cos^2\\phi'}}
        \\end{equation$head}";
    else
        out="\\begin{multline$head}
        \\frac{K_p}{\\cos\\alpha}=\\frac{1}{\\cos^2\\phi'}
        \\Bigg(2\\cos^2\\alpha+2\\frac{c'}{\\gamma{z}}\\cos\\phi'\\sen\\phi'\\\\
        +\\sqrt{4\\cos^2\\alpha(\\cos^2\\alpha-\\cos^2\\phi')+
        4\\left(\\frac{c'}{\\gamma{z}}\\right)^2\\cos^2\\phi'+8\\frac{c'}{\\gamma{z}}
        \\cos^2\\alpha\\sen\\phi'\\cos\\phi'}\\Bigg)-1
        \\end{multline$head}";
    end
    return out;
end

function kr_maku_equation_lcode(;wn::Int64=0)
    head="*"
    if wn!=0
        head=""
    end
    out="\\begin{equation$head}
    K_o=(1-\\sen\\phi')OCR^{\\sen\\phi'}
    \\end{equation$head}"
end

function kr_sch_equation_lcode(;wn::Int64=0)
    head="*"
    if wn!=0
        head=""
    end
    out="\\begin{equation$head}
    K_o=0.5\\left(OCR\\right)^{0.5}
    \\end{equation$head}"
end

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

function draw_soil_surface_lcode(model::Wmodel{<:Real},offs::Real=0)
    out="";
    #linea de la superficie derecha
    #obteniendo coordenadas del nudo superior derecho
    #el nudo en la fila 10 en model.nod es el nudo superior derecho del muro
    nod=model.nod;
    maxy=nod[10,2];
    maxx=nod[10,1];
    #obteniendo vector con componente horizontal unitaria en la dirección de la
    #pendiente del terreno
    alpha=deg2rad(model.alpha);
    uy=tan(alpha);#ux=1, implícito
    #maximo valor de x, en la matriz de nudos
    B=maximum(model.nod[:,1]);
    #desplazando
    B+=offs;
    uy*=B;
    out*="\\draw ($(maxx),$(maxy))--($(maxx+B),$(maxy+uy));
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
    out*="\\draw (-.5,$(model.D))--($(x2),$(model.D));"
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
