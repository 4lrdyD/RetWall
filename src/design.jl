#revisión 0.0.0 19-07-2023, 23:25 Julia 1.6.4
export long_reinforcement_area;
"""
    long_reinforcement_area(Mu::Real,phi::Real,fy::Real,d::Real,a::Real)

Devuelve el área de refuerzo longitudinal

*   `Mu`: Momento último
*   `phi`: Factor de reducción de resistencia.
*   `fy`: Límite de fluencia del acero.
*   `d`: distancia desde la fibra extrema en compresión hasta el centroide del refuerzo
    longitudinal en tracción.
*   `a`: profundidad de la zona en compresión
"""
function long_reinforcement_area(Mu::Real,phi::Real,fy::Real,d::Real,a::Real)
    if Mu<0 || phi<0 || fy<0 || d<0 || a<0
        error("los parámetros deben ser positivos");
    else
        if d<a error("d debe ser mayor que a") end
    end
    Mu/(phi*fy*(d-a/2));
end
