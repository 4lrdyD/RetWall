#revisión 0.0.0 29-01-2020, 00:40 Julia1.1.0
function report(x)
    a="
    \\documentclass[oneside,spanish]{scrbook}
    \\begin{document}
      Hola $x
      Bola
    \\end{document}
"
open("prueba1.tex", "w") do f
           write(f, a)
           end
run(`pdflatex prueba1`);
run(`cmd /c start prueba1.pdf`);
end
