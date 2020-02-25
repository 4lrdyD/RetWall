#revisión 0.0.9 24-02-2020, 22:15 Julia1.1.0
export Wmodel, gravity_wall,addsoil!, addmat!,wall_forces
mutable struct Wmodel{T}
    #nudos para el muro
    nod::VolatileArray{T,2}

    #elementos del muro, se forman con los nudos
    elm::VolatileArray{Int64,2}

    #índice del último elemento triangular en elm, los siguientes serán
    #elementos cuadrangulares
    pbreak::Int64

    #propiedades de los materiales un material por fila
    matprop::VolatileArray{T,2}

    #nudos especiales para la línea de presiones
    pnod::VolatileArray{T,2}

    #* linea de presiones, se forman con los nudos en pnod una
    #fila->[n1 n2 prop type side]; n1, n2 índices de nudos en pnod; prop el
    #índice de la propiedad de suelo en soilprop; type: 0->presión activa,
    #1->presión en reposo y 2->presión pasiva; side 0-> la presión actua de
    # izquierda a derecha y 1->de derecha a izquierda.
    #* pliners: linea a la derecha del muro
    #* plinels: linea a la izquierda del muro
    pliners::VolatileArray{Int64,2}
    plinels::VolatileArray{Int64,2}

    #propiedades de suelos un tipo de suelo por fila
    soilprop::VolatileArray{T,2}

    #ángulo de inclinación del terreno en grados sexagesimales
    alpha::Real

    #profundidad de desplante de la cimentación del muro
    D::Real

    #fuerzas del muro (peso de cada región y )
    function Wmodel(nod::VolatileArray{T,2}, elm::VolatileArray{Int64,2},
        pbreak::Int64) where {T<:Real}
            soilprop=VolatileArray(zeros(0,3));
            matprop=VolatileArray(zeros(0,3));
            pnod=VolatileArray(zeros(0,2));
            pliners=VolatileArray(zeros(Int64,0,5));
            plinels=VolatileArray(zeros(Int64,0,5));
            new{T}(nod,elm,pbreak,matprop,pnod,pliners,plinels,soilprop,0,-1);
    end
end
function Base.show(io::IO,x::Wmodel{<:Real})
    print(io,"$(typeof(x))\n");
    print(io,"Fields:\n")
    print(io,"   nod: $(size(x.nod)[1])x$(size(x.nod)[2]) $(typeof(x.nod))\n");
    print(io,"   elm: $(size(x.elm)[1])x$(size(x.elm)[2]) $(typeof(x.elm))\n");
    print(io,"pbreak: $(x.pbreak) $(typeof(x.pbreak))");
end

function wall_forces(model::Wmodel{T}) where {T<:Real}
    nel=size(model.elm)[1];
    #matriz de fuerzas
    prop=VolatileArray(zeros(T,0),0,0);
    prop[nel,3]=0;

    #obteniendo tres primeras filas área, xm , ym
    build_wall(Array(model.nod),Array(model.elm),Array(prop),
        model.pbreak);

    #obteniendo peso y momento respecto al origen
    for i in 1:nel
        @inbounds prop[i,4]=prop[i,1]*model.matprop[model.elm[i,5],3];
        @inbounds prop[i,5]=prop[i,4]*prop[i,2];
    end
    return prop;
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

    return Wmodel(nodes,elements,2);
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
aparente (Kpa) y el peso unitario del suelo (KN/m3). Alternativamente puede
ingresarse el ángulo de fricción entre el suelo y el muro (4ta columna) y la
capacidad portante del terreno (5ta columna).

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

function build_rankine_pline(model::Wmodel{<:Real})
    #la línea se trazará desde la parte más baja del muro (zapata)
    #verticalmente hasta intersectar con el suelo, por defecto se considerará
    #la parte derecha como la parte posterior del muro (en contacto con el
    #terreno), y la parte izquierda la parte frontal (donde se considerará
    #empuje pasivo).

    #obteniendo coordenadas del nudo superior derecho
    nod=model.nod;
    maxy=maximum(nod[:,2]);
    maxids=findall(isequal(maxy),nod[:,2]);
    maxx=maximum(nod[maxids,1]);

    #obteniendo coordenadas de los nudos inferior derecho e izquierdo
    miny=minimum(nod[:,2]);
    minids=findall(isequal(miny),nod[:,2]);
    mindx=maximum(nod[minids,1]);#x inferior derecho
    minix=minimum(nod[minids,1]);#x inferior izquierdo

    #obteniendo nudo superior de la linea de empuje activo
    ra=deg2rad(model.alpha);
    maxdy=maxy+(mindx-maxx)*tan(ra);

    #obteniendo nudo superior de la linea de empuje pasivo
    merror="Debe ingresarse una profundidad de cimentación (campo D)"
    model.D>0 ? maxiy=minix+model.D : error(merror);

    #agregando nudos
    model.pnod=VolatileArray([mindx miny;mindx maxdy;minix miny;minix maxiy]);

    #agregando elementos
    model.plinels=VolatileArray([4 3 1 2 0]);
    model.pliners=VolatileArray([2 1 1 0 1]);
end

"""
    build_wall(nodes::Array{T,2},
        elm::Array{<:Integer,2},elmp::Array{T,2},
        pbreak::Integer) where {T<:Real}
Constructor del muro, calculará el área y el centro de gravedad de todos los
elementos.
*   `nodes`: matriz de nudos, cada fila de esta matriz deberá contener las
    coordenadas `[xi yi]` de un nudo, el índice `i` de un nudo es determinado
    por su ubicación en esta matriz.
*   `elm`: matriz de elementos, cada fila de est matriz deberá contener los
    índices `[id1 id2 id3 id4]` de los nudos que forman el elemento, estos
    índices deberán ser consistentes con la matriz de nudos (`nodes`). para
    elementos triangulares `id4=0`.
*   `elmp`: matriz de propiedades de los elementos, está matriz deberá tener
    al menos 3 columnas y al menos igual cantidad de filas que `elm`, por cada
    elemento en `elm`, se rellenará la fila correspondiente con `[Ai xmi ymi]`,
    que son el área y los componentes `x` e `y` del centro de gravedad.
*   `pbreak`: ubicación en `elm`, del último elemento correspondiente a
    elementos triangulares.

"""
function build_wall(nodes::Array{T,2},
    elm::Array{<:Integer,2},elmp::Array{T,2},pbreak::Integer) where {T<:Real}
    #número de elementos en cada matriz
    nel=size(elm)[1];

    #pbreak indica el punto donde terminan los elementos
    #triangulares
    if pbreak>0
        for i in 1:pbreak
            build_tri(nodes,elm,elmp,i);
        end
    end

    if pbreak<nel
        for i in pbreak+1:nel
            buil_quad(nodes,elm,elmp,i);
        end
    end
    return elmp;
end

function build_tri(nodes::Array{T,2},
    elm::Array{<:Integer,2},elmp::Array{T,2},id::Integer) where {T<:Real}
    #nudos (índices)
    @inbounds n1=elm[id,1];
    @inbounds n2=elm[id,2];
    @inbounds n3=elm[id,3];

    #nudos (coordenadas)
    @inbounds x1=nodes[n1,1];
    @inbounds y1=nodes[n1,2];
    @inbounds x2=nodes[n2,1];
    @inbounds y2=nodes[n2,2];
    @inbounds x3=nodes[n3,1];
    @inbounds y3=nodes[n3,2];

    #baricentro
    @inbounds elmp[id,2]=(x1+x2+x3)/3;
    @inbounds elmp[id,3]=(y1+y2+y3)/3;

    #área
    @inbounds elmp[id,1]=(x2*y3-y2*x3-x1*y3+y1*x3+
                    x1*y2-y1*x2)/2;
end

function buil_quad(nodes::Array{T,2},
    elm::Array{<:Integer,2},elmp::Array{T,2},id::Integer) where {T<:Real}
    #nudos (índices)
    @inbounds n1=elm[id,1];
    @inbounds n2=elm[id,2];
    @inbounds n3=elm[id,3];
    @inbounds n4=elm[id,4];

    #nudos (coordenadas)
    @inbounds x1=nodes[n1,1];
    @inbounds y1=nodes[n1,2];
    @inbounds x2=nodes[n2,1];
    @inbounds y2=nodes[n2,2];
    @inbounds x3=nodes[n3,1];
    @inbounds y3=nodes[n3,2];
    @inbounds x4=nodes[n4,1];
    @inbounds y4=nodes[n4,2];

    #áreas de los componentes triangulares
    @inbounds A1=(x2*y3-y2*x3-x1*y3+y1*x3+
                    x1*y2-y1*x2)/2;
    @inbounds A2=(x3*y4-y3*x4-x1*y4+y1*x4+
                    x1*y3-y1*x3)/2;
    @inbounds elmp[id,1]=A1+A2;

    #baricentros
    @inbounds xm1=(x1+x2+x3)/3;
    @inbounds ym1=(y1+y2+y3)/3;
    @inbounds xm2=(x1+x3+x4)/3;
    @inbounds ym2=(y1+y3+y4)/3;
    @inbounds elmp[id,2]=(A1*xm1+A2*xm2)/elmp[id,1];
    @inbounds elmp[id,3]=(A1*ym1+A2*ym2)/elmp[id,1];
end

function soil_rankine_forces_rs(model::Wmodel{<:Real})
    nel=size(model.pliners)[1];
    #declarando el objeto de salida
    #será una matriz de nelx4, por cada fila los datos serán
    #fuerza en la dirección horizontal, fuerza en la dirección vertical,
    #brazo horizontal y brazo vertical; inicialmente la matriz será vacia (0x0)
    #los elementos se irán ingresando según se calculen los valores
    #correspondientes.
    out=VolatileArray(zeros(0,0));
    qload=0.0;
    for i in 1:nel
        #coordenadas de los nudos
        n1=model.pliners[i,1];
        n2=model.pliners[i,2];
        xu=model.pnod[n1,1];
        yu=model.pnod[n1,2];
        err="Las coordenadas en y no son consistentes con el estrato anterior";
        if i!=1
            if yu!=yd#validación necesaria de las coordenadas en y
                error(err);
            end
        end
        xd=model.pnod[n2,1];
        yd=model.pnod[n2,2];
        #obteniendo la altura del estrato y validando el valor
        h=yu-yd;
        err="la línea resultante para aplicar la teoría de presión de tierra "
        err1="de Rankine, debe ser vertical y positiva en todos los estratos"
        if h<=0 || xu-xd!=0
            error(err*err1);
        end
        #obteniendo propiedades
        pid=model.pliners[i,3];
        fi=model.soilprop[pid,1];
        c=model.soilprop[pid,2];
        gamma=model.soilprop[pid,3];
        #la utilización de suelo con cohesión solo será permitida para el
        #primer estrato (por ahora)
        fr=deg2rad(fi);
        ar=deg2rad(model.alpha);
        if i==1 && c!=0
            #profundidad de la grieta de tensión
            zc=2*c*sqrt((1+sin(fr))/(1-sin(fr)))/gamma;
            ka=ka_rankine(fi,model.alpha,c,gamma,h);
        else
            if c!=0
                err="Por ahora, la cohesión solo es permitida para el primer "
                err1="estrato, se ingnorará la cohesión en los demás estratos."
                print(err*err1);
            end
            ka=ka_rankine(fi,model.alpha);
            pa=0.5*gamma*ka*h^2;
            paq=ka*h*qload/cos(ar);
            pat=pa+paq;
            qload+=gamma*h;
            out[i,1]=pa*cos(ar);#fuerza horizontal
            out[i,2]=pa*sin(ar);#fuerza vertical
            out[i,3]=xu;#razo horizontal
            out[i,4]=yd+(pa*h/3+paq*h/2)/pat;#brazo vertical
        end
    end
end
