#revisión 0.0.6 04-03-2020, 22:40 Julia1.1.0
function report(hp,hz,t1,t2,t3,b1,b2,grav,prop)
a="
\\documentclass[oneside,spanish]{scrbook}
\\usepackage[spanish, es-nodecimaldot, es-tabla]{babel}
\\usepackage{amsmath}
\\usepackage{tikz}
\\usepackage{pgfplots}
\\pgfplotsset{compat=newest}
\\usepgfplotslibrary{units}
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

function draw_elm_label_lcode(prop::VolatileArray{<:Real,2})
    #debe ser insertado dentro de un entorno tikzpicture
    out="";
    for i in 1:size(prop)[1]
        out=out*"\\draw ($(prop[i,2]),$(prop[i,3]))node{\\small{$i}};
        "
    end
    return out;
end

function draw_polyline_lcode(joints::Array{<:Real,2},args::Int64...;
    ops::String="",close::Int64=0)
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

function kp_rankine_equation_lcode()
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
