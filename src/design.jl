#revisión 0.0.1 21-07-2023, 00:31 Julia 1.9.2
export long_reinforcement_area, compression_zone_depth;
"""
    long_reinforcement_area(Mu::Real,phi::Real,fy::Real,d::Real,a::Real)

Devuelve el área de refuerzo longitudinal

*   `Mu`: Momento último
*   `phi`: Factor de reducción de resistencia.
*   `fy`: Límite de fluencia del acero.
*   `d`: distancia desde la fibra extrema en compresión hasta el centroide del refuerzo
    longitudinal en tracción.
*   `a`: profundidad de la zona en compresión
Considerar unidades consistentes
"""
function long_reinforcement_area(Mu::Real,phi::Real,fy::Real,d::Real,a::Real)
    if Mu<0 || phi<0 || fy<0 || d<0 || a<0
        error("los parámetros deben ser positivos");
    else
        if d<a error("d debe ser mayor que a") end
    end
    Mu/(phi*fy*(d-a/2));
end

"""
    function compression_zone_depth(As::Real,fy::Real,fc::Real,b::Real)

Devuelve la profundida de zona la en compresión

*   `As`: Área de refuerzo longitudinal
*   `fy`: Límite de fluencia del acero.
*   `fc`: Resistencia a la compresión del concreto
*   `b`: ancho de la sección
Considerar unidades consistentes
"""
function compression_zone_depth(As::Real,fy::Real,fc::Real,b::Real)
    if As<0 || fy<0 || fc<0 || b<0
        error("los parámetros deben ser positivos");
    end
    As*fy/(0.85*fc*b);
end
