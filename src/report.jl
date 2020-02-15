#revisión 0.0.2 15-02-2020, 01:00 Julia1.1.0
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
    \\begin{tikzpicture}

      \\begin{axis}[
          width=\\linewidth, yticklabels=\\empty,xticklabels=\\empty,
            ytick=\\empty,xtick=\\empty,axis line style={draw=none}
        ]
        $(draw_wall_lcode(grav))
        $(draw_elm_label_lcode(prop))
      \\end{axis}
    \\end{tikzpicture}
  \\caption{Muro de contención de gravedad}
	\\label{fig:spectre1}
\\end{figure}
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
    out="";
    #número de elementos
    nel=size(model.elm)[1];

    #pbreak indica el punto donde terminan los elementos
    #triangulares
    if model.pbreak>0
        for i in 1:model.pbreak
            out=out*"\\addplot[black=50!]
            table[x=x,y=y,col sep=&,row sep=\\\\] {x & y\\\\
            $(model.nod[model.elm[i,1],1]) & $(model.nod[model.elm[i,1],2])\\\\
            $(model.nod[model.elm[i,2],1]) & $(model.nod[model.elm[i,2],2])\\\\
            $(model.nod[model.elm[i,3],1]) & $(model.nod[model.elm[i,3],2])\\\\
            $(model.nod[model.elm[i,1],1]) & $(model.nod[model.elm[i,1],2])\\\\};
            "
        end
    end

    if model.pbreak<nel
        for i in model.pbreak+1:nel
            out=out*"\\addplot[black=50!]
            table[x=x,y=y,col sep=&,row sep=\\\\] {x & y\\\\
            $(model.nod[model.elm[i,1],1]) & $(model.nod[model.elm[i,1],2])\\\\
            $(model.nod[model.elm[i,2],1]) & $(model.nod[model.elm[i,2],2])\\\\
            $(model.nod[model.elm[i,3],1]) & $(model.nod[model.elm[i,3],2])\\\\
            $(model.nod[model.elm[i,4],1]) & $(model.nod[model.elm[i,4],2])\\\\
            $(model.nod[model.elm[i,1],1]) & $(model.nod[model.elm[i,1],2])\\\\};
            "
        end
    end
    return out;
end

function draw_elm_label_lcode(prop::VolatileArray{<:Real,2})
    out="";
    for i in 1:size(prop)[1]
        out=out*"\\draw ($(prop[i,2]),$(prop[i,3]))node{\\small{$i}};
        "
    end
    return out;
end
