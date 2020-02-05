#revisión 0.0.3 05-02-2020, 01:50 Julia1.1.0
export Wmodel, gravity_wall,addsoil!, addmat!
mutable struct Wmodel{T}
    nod::VolatileArray{T,2}
    elm::VolatileArray{Int64,2}
    prop::VolatileArray{T,2}
    pbreak::Int64
    soilprop::VolatileArray{T,2}
    matprop::VolatileArray{T,2}
    pline::VolatileArray{Int64,2}

    function Wmodel(nod::VolatileArray{T,2}, elm::VolatileArray{Int64,2},
        prop::VolatileArray{T,2},pbreak::Int64) where {T<:Real}
        if size(prop)[1]==size(elm)[1]
            soilprop=VolatileArray(zeros(0,3));
            matprop=VolatileArray(zeros(0,3));
            pline=VolatileArray(zeros(Int64,0,3));
            new{T}(nod,elm,prop,pbreak,soilprop,matprop,pline);
        else
            error("las alturas de elm y prop deben ser iguales")
        end
    end
end
function Base.show(io::IO,x::Wmodel{<:Real})
    print(io,"$(typeof(x))\n");
    print(io,"Fields:\n")
    print(io,"   nod: $(size(x.nod)[1])x$(size(x.nod)[2]) $(typeof(x.nod))\n");
    print(io,"   elm: $(size(x.elm)[1])x$(size(x.elm)[2]) $(typeof(x.elm))\n");
    print(io,"  prop: $(size(x.prop)[1])x$(size(x.prop)[2]) $(typeof(x.prop))\n");
    print(io,"pbreak: $(x.pbreak) $(typeof(x.pbreak))");
end
"""
    gravity_wall(;hp::Real, hz::Real, t1::Real, t2::Real, t3::Real,
        b1::Real,b2::Real)
Genera una estructura del tipo `Wproperties`, para un muro de gravedad con los
datos especificados, los datos deben ser ingresados como `keywords`, es decir,
pueden ingresarse en cualquier orden, pero especificando el nombre la variable,
por ejemplo:
`gravity_wall(b1=0.95,b2=0.3,hp=5.7,hz=0.8,t3=1.6,t2=0.3,t1=0.6)`.

                  t1
                 ____            _
                /|  |\\           .
               / |  | \\          .    relleno
              /  |  |  \\        hp
             /   |  |   \\        .
            /    |  |    \\       .
      b1   /  t2 |  |  t3 \\  b2  .
     _____/______|__|______\\______
    |                             |hz
    |_____________________________|

"""
function gravity_wall(;hp::Real, hz::Real, t1::Real, t2::Real, t3::Real,
    b1::Real,b2::Real)
    #creando nudos
    nodes=VolatileArray(zeros(1,2));
    push!(nodes,Float64[b1+t2+t1+t3+b2 0]);
    push!(nodes,Float64[b1+t2+t1+t3+b2 hz]);
    push!(nodes,Float64[0 hz]);
    push!(nodes,Float64[b1 hz]);
    push!(nodes,Float64[b1+t2 hz]);
    push!(nodes,Float64[b1+t2+t1 hz]);
    push!(nodes,Float64[b1+t2+t1+t3 hz]);
    push!(nodes,Float64[b1+t2 hz+hp]);
    push!(nodes,Float64[b1+t2+t1 hz+hp]);

    #creando elementos, el quinto elemento corresponde al material
    elements=VolatileArray([5 6 9 0 1]);#triángulo izquierdo
    push!(elements,[7 8 10 0 1]);#triángulo derecho
    push!(elements,[1 2 3 4 1]);#cuadrilátero base (zapata)
    push!(elements,[6 7 10 9 1]);#cuadrilatero central (en pantalla)

    props=VolatileArray(zeros(4,3));

    #construyendo muro (obteniendo áreas y centroides)
    #el 2 indica el índice de la fila del último elemento triangular
    build_wall(Array(nodes),Array(elements),Array(props),2);

    return Wmodel(nodes,elements,props,2);
end

function add_field_row(field::VolatileArray{T,2},prop::Array{T,N}) where {
    T<:Real,N}
    dimy=size(field)[1];
    dimx=size(field)[2];
    ly=size(prop)[1];
    lx=length(size(prop))==1 ? 1 : size(prop)[2];

    if lx==dimx
        #adjuntando propiedad
        append!(field,prop,dim=1);
    elseif lx>dimx
        #redimensionando
        field[dimy+ly,lx]=0;
        #adjuntando propiedad
        field[dimy+1:dimy+ly,1:lx]=prop;
    elseif lx<dimx
        #redimensionando
        field[dimy+ly,dimx]=0;
        #copiando elementos en el lugar correspondiente
        field[dimy+1:dimy+ly,1:lx]=prop;
    else
        error("dimensiones no compatibles");
    end
    return dimy+1:dimy+ly;
end

"""
    addsoil!(model::Wmodel{T},prop::Array{T,N}) where {T<:Real,N}
Agrega una o varias propiedades de suelo al modelo (`model`), las tres primeras
columnas de `prop` deberán ser en ése orden el ángulo de fricción por esfuerzo
efectivo del suelo en grados sexagesimales, la resistencia no drenada o cohesión
aparente (Kpa) y el peso unitario del suelo (KN/m3).

Devuelve el rango de índices correspondientes a las propiedades agregadas.
"""
addsoil!(model::Wmodel{T},prop::Array{T,N}) where {T<:Real,N}=
    add_field_row(model.soilprop,prop);

"""
    addmat!(model::Wmodel{T},prop::Array{T,N}) where {T<:Real,N}
Agrega una o varias propiedades de material al modelo (`model`), las tres
primeras columnas de `prop` deberán ser en ése orden la resistencia a la
compresión (MPa), resistencia a la tracción (MPa) y el peso específico (KN/m3).

Devuelve el rango de índices correspondientes a las propiedades agregadas.
"""
addmat!(model::Wmodel{T},prop::Array{T,N}) where {T<:Real,N}=
    add_field_row(model.matprop,prop);

function build_pline(model::Wmodel{<:Real},D::Real,alpha::Real)

end
