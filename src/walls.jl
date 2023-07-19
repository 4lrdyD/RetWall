#revisión 0.2.0 18-07-2023, 23:54 Julia1.6.4
export Wmodel, typeIwall, gravity_wall,addsoil!, addmat!,wall_forces,
        soil_rankine_forces_rs,soil_rankine_forces_ls,check_stab_wt1,
        uload_rankine_forces_rs, uload_rankine_forces_ls, combine_soil_forces,
        orient_model!, rebuild!
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

    function Wmodel(nod::VolatileArray{T,2}, elm::VolatileArray{Int64,2},
        pbreak::Int64) where {T<:Real}
            soilprop=VolatileArray(zeros(0,3));
            matprop=VolatileArray(zeros(0,3));
            pnod=VolatileArray(zeros(0,2));
            pliners=VolatileArray(zeros(Int64,0,5));
            plinels=VolatileArray(zeros(Int64,0,5));
            new{T}(nod,elm,pbreak,matprop,pnod,pliners,plinels,soilprop);
    end
end

function Base.copy(model::Wmodel{<:Real})
    nods=copy(model.nod);
    elms=copy(model.elm);
    out=Wmodel(nods,elms,model.pbreak);

    out.matprop=copy(model.matprop);
    out.pnod=copy(model.pnod);
    out.pliners=copy(model.pliners);
    out.plinels=copy(model.plinels);
    out.soilprop=copy(model.soilprop);
    return out;
end

mutable struct typeIwall
    hp::Real
    hz::Real
    t1::Real
    t2::Real
    t3::Real
    b1::Real
    b2::Real
    #ángulo de inclinación del terreno en grados sexagesimales
    alpha::Real
    #profundidad de desplante de la cimentación del muro (m)
    D::Real
    #carga distribuida KN/m2
    q::Real
    model::Wmodel{<:Real}
    function typeIwall(;hp::Real, hz::Real, t1::Real, t2::Real, t3::Real,
        b1::Real,b2::Real)
        model=gravity_wall(;hp=hp, hz=hz, t1=t1, t2=t2, t3=t3, b1=b1, b2=b2);
        new(hp,hz,t1,t2,t3,b1,b2,0,-1,0,model);
    end
end

function Base.show(io::IO,x::Wmodel{<:Real})
    print(io,"$(typeof(x))\n");
    print(io,"Fields:\n")
    print(io,"   nod: $(size(x.nod)[1])x$(size(x.nod)[2]) $(typeof(x.nod))\n");
    print(io,"   elm: $(size(x.elm)[1])x$(size(x.elm)[2]) $(typeof(x.elm))\n");
    print(io,"pbreak: $(x.pbreak) $(typeof(x.pbreak))");
end

function Base.show(io::IO,x::typeIwall)
    print(io,"$(typeof(x))\n");
    print(io,"hp=$(x.hp)\n");
    print(io,"hz=$(x.hz)\n");
    print(io,"t1=$(x.t1)\n");
    print(io,"t2=$(x.t2)\n");
    print(io,"t3=$(x.t3)\n");
    print(io,"b1=$(x.b1)\n");
    print(io,"b2=$(x.b2)\n");
    print(io,"alpha=$(x.alpha)\n");
    print(io,"D=$(x.D)\n");
    print(io,"q=$(x.q)\n");
    print(io,"                      t1
                     ____            _
                    /|  |\\           .
                   / |  | \\          .    relleno
                  /  |  |  \\        hp
                 /   |  |   \\        .
                /    |  |    \\       .
          b1   /  t2 |  |  t3 \\  b2  .
         _____/______|__|______\\______
        |                             |hz
        |_____________________________|")
end

function rebuild!(x::typeIwall)
    hp=x.hp;
    hz=x.hz;
    t1=x.t1;
    t2=x.t2;
    t3=x.t3;
    b1=x.b1;
    b2=x.b2;
    gravity_wall(;hp=hp, hz=hz, t1=t1, t2=t2, t3=t3, b1=b1, b2=b2,
        model=x.model);
    return x;
end

"""
    orient_model!(model::Wmodel{<:Real})
Orientará todos los elementos (campo `elm`).
"""
function orient_model!(model::Wmodel{<:Real})
    nod=model.nod;
    elm=model.elm;
    cont=1;
    #recorriendo elementos triangulares
    for i in 1:model.pbreak
        key=orient_tri!(nod,elm,cont);
        #key=0 indica que no se eliminó el elemento y
        #puede avanzarse a la siguiente ubicación, key=-1 indica que se
        #eliminó el elemento, cont se mantiene
        if key==0 cont+=1 end
    end
    #actualizando campo pbreak
    model.pbreak=cont-1;
    nel=size(elm)[1];
    #recorriendo elementos cuadrangulares
    for i in cont:nel
        key=orient_quad!(nod,elm,cont,model.pbreak);
        #key=-1 indica que el elemento fue elminado
        #key=1, se transforma el elemento a triángulo, se elimina el cuadrilátero
        #pero se agrega un triangulo
        #no se realizó ningún cambio (key=0).
        if key==1
            model.pbreak+=1;
            cont+=1;
        elseif key==0;
            cont+=1;
        end
    end
    return elm;
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
    lmp=size(model.matprop)[1];
    if lmp==0
        error("No se ha ingresado ningún material")
    end
    for i in 1:nel
        sid=model.elm[i,5];
        if sid>lmp
            error("No existe la propiedad de suelo al que se hace referencia");
        end
        @inbounds prop[i,4]=prop[i,1]*model.matprop[model.elm[i,5],3];
        @inbounds prop[i,5]=prop[i,4]*prop[i,2];
    end
    return prop;
end

"""
    gravity_wall(;hp::Real, hz::Real, t1::Real, t2::Real, t3::Real,
        b1::Real,b2::Real,kwargs...)
Genera una estructura del tipo `Wmodel`, para un muro de gravedad con los
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
    b1::Real,b2::Real,kwargs...)
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

    if haskey(kwargs,:model)
        model=kwargs[:model];
        if typeof(model)==Wmodel{Float64}
            model.pbreak=2;
            model.nod=nodes;
            model.elm=elements;
            return model;
        else
            error("El modelo no tiene el tipo esperado");
        end
    else
        return Wmodel(nodes,elements,2);
    end
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
Alternativamente puede ingresarse la resistencia a la fluencia fy  (en MPa 4ta columna).

Devuelve el rango de índices correspondientes a las propiedades agregadas.
"""
addmat!(model::Wmodel{T},prop::Array{T,N}) where {T<:Real,N}=
    add_field_row(model.matprop,prop);

"""
    build_rankine_pline(wall::typeIwall)
Construye la linea de presiones para un análisis por la teoria de empuje de
suelo de Rankine, la línea se trazará desde la parte más baja del muro (zapata)
verticalmente hasta intersectar con el suelo, por defecto se considerará
la parte derecha como la parte posterior del muro (en contacto con el
terreno), y la parte izquierda la parte frontal (donde se considerará
empuje pasivo).

Llenara los campos, `pnod`, `plinels` y `pliners` de `wall.model`
"""
function build_rankine_pline(wall::typeIwall)
    #la línea se trazará desde la parte más baja del muro (zapata)
    #verticalmente hasta intersectar con el suelo, por defecto se considerará
    #la parte derecha como la parte posterior del muro (en contacto con el
    #terreno), y la parte izquierda la parte frontal (donde se considerará
    #empuje pasivo).

    #obteniendo coordenadas del nudo superior derecho
    model=wall.model;
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
    ra=deg2rad(wall.alpha);
    maxdy=maxy+(mindx-maxx)*tan(ra);

    #obteniendo nudo superior de la linea de empuje pasivo
    merror="Debe ingresarse una profundidad de cimentación (campo D)"
    wall.D>0 ? maxiy=miny+wall.D : error(merror);

    #agregando nudos
    model.pnod=VolatileArray([mindx miny;mindx maxdy;minix miny;minix maxiy]);

    #agregando elementos
    model.plinels=VolatileArray([4 3 1 2 0]);
    model.pliners=VolatileArray([2 1 1 0 1]);
end

"""
    build_coulomb_pline(wall::typeIwall)
Construye la linea de presiones para un análisis por la teoria de empuje de
suelo de Coulomb, la línea se trazará desde la parte más baja del muro (zapata)
de modo que la línea generada a la derecha del muro, coincida con la línea
de la cara posterior del muro, por defecto se considerará
la parte derecha como la parte posterior del muro (en contacto con el
terreno), y la parte izquierda la parte frontal (donde se considerará
empuje pasivo).

Llenara los campos, `pnod`, `plinels` y `pliners` de `wall.model`
"""
function build_coulomb_pline(wall::typeIwall)
    model=wall.model;
    #obteniendo nudos
    nod=wall.model.nod;
    #obteniedo el angulo que forma la cara posterior con la vertical
    angle=atan(wall.t3,wall.hp);
    #mensaje de error en caso D<0
    merror="Debe ingresarse una profundidad de cimentación (campo D)"

    #agregando nudos
    model.pnod=VolatileArray([nod[8,1]+wall.hz*tan(angle) 0;nod[10:10,:];
                            0 0;0 wall.D>0 ? wall.D : error(merror)]);

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

function soil_rankine_forces_rs(model::Wmodel{<:Real};alpha::Real=0)
    nel=size(model.pliners)[1];
    #declarando el objeto de salida
    #será una matriz de nelx6, por cada fila los datos serán
    #fuerza en la dirección horizontal, fuerza en la dirección vertical,
    #brazo horizontal, brazo vertical, Momento producido por la fuerza
    #horizontal, momento producido por la fuerza vertical y el coeficiente de
    #presión.
    #Inicialmente la matriz será vacia (0x0)
    #los elementos se irán ingresando según se calculen los valores
    #correspondientes.
    out=VolatileArray(zeros(0,0));
    qload=0.0;#para un estrato inferior, los estratos superiores actuarán como
              #carga distribuida sobre este.

    #declarando yd;
    yd=0.0;
    for i in 1:nel
        #coordenadas de los nudos
        n1=model.pliners[i,1];
        n2=model.pliners[i,2];
        xu=model.pnod[n1,1];
        yu=model.pnod[n1,2];
        err="Las coordenadas en `y` no son consistentes con el estrato anterior";
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
        if h<=0 || xu-xd>1e6
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
        ar=deg2rad(alpha);
        if i==1 && c!=0
            #profundidad de la grieta de tensión
            zc=2*c*sqrt((1+sin(fr))/(1-sin(fr)))/gamma;
            ka=ka_rankine(fi,model.alpha,c,gamma,h);
            hc=h-zc;
            pa=0.5*gamma*ka*hc^2;
            out[i,1]=pa*cos(ar);#fuerza horizontal
            out[i,2]=pa*sin(ar);#fuerza vertical
            out[i,3]=xu;#razo horizontal
            out[i,4]=yd+hc/3;#brazo vertical
            out[i,5]=out[i,1]*out[i,4];#momento de la fuerza horizontal
            out[i,6]=out[i,2]*out[i,3];#momento de la fuerza vertical
            out[i,7]=ka;#coeficiente de presión
        else
            if c!=0
                err="Por ahora, la cohesión solo es permitida para el primer "
                err1="estrato, se ingnorará la cohesión en los demás estratos."
                print(err*err1);
            end
            ka=ka_rankine(fi,alpha);
            pa=0.5*gamma*ka*h^2;
            paq=ka*h*qload/cos(ar);
            pat=pa+paq;
            out[i,1]=pat*cos(ar);#fuerza horizontal
            out[i,2]=pat*sin(ar);#fuerza vertical
            out[i,3]=xu;#brazo horizontal
            out[i,4]=yd+(pa*h/3+paq*h/2)/pat;#brazo vertical
            out[i,5]=out[i,1]*out[i,4];#momento de la fuerza horizontal
            out[i,6]=out[i,2]*out[i,3];#momento de la fuerza vertical
            out[i,7]=ka;#coeficiente de presión
        end
        qload+=gamma*h;
    end
    return out;
end

function soil_rankine_forces_ls(model::Wmodel{<:Real})
    nel=size(model.plinels)[1];
    #declarando el objeto de salida
    #será una matriz de nelx6, por cada fila los datos serán
    #fuerza en la dirección horizontal, fuerza en la dirección vertical,
    #brazo horizontal, brazo vertical, momento producido por la fuerza
    #horizontal, momento producido por la fuerza vertical y el coeficiente
    #de presión.
    #Inicialmente la matriz será vacia (0x0)
    #los elementos se irán ingresando según se calculen los valores
    #correspondientes.
    out=VolatileArray(zeros(0,0));
    qload=0.0;#para un estrato inferior, los estratos superiores actuarán como
              #carga distribuida sobre este.

    #declarando yd;
    yd=0.0;
    for i in 1:nel
        #coordenadas de los nudos
        n1=model.plinels[i,1];
        n2=model.plinels[i,2];
        xu=model.pnod[n1,1];
        yu=model.pnod[n1,2];
        err="Las coordenadas en `y` no son consistentes con el estrato anterior";
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
        if h<=0 || xu-xd>1e6
            error(err*err1);
        end
        #obteniendo propiedades
        pid=model.plinels[i,3];
        fi=model.soilprop[pid,1];
        c=model.soilprop[pid,2];
        gamma=model.soilprop[pid,3];
        #la utilización de suelo con cohesión solo será permitida para el
        #primer estrato (por ahora)
        fr=deg2rad(fi);
        if i==1 && c!=0
            #calculando presión en la parte superior
            #la función para el coeficiente de presión no admite z=0, por tanto
            #reemplazamos z=1e-6 para obtener el valor aproximado en la parte
            #superior.
            pu=1e-6*gamma*kp_rankine(fi,0,c,gamma,1e-6);
            #presión en la parte inferior z=h
            pd=h*gamma*kp_rankine(fi,0,c,gamma,h);
            #pu y pd determinan un trapecio del cual tenemos que obtener el área
            #y el brazo vertical área=fuerza
            pp=(pu+pd)*h/2;
            out[i,1]=pp;#fuerza horizontal
            out[i,3]=xu;#brazo horizontal
            out[i,4]=((pu*h)*h/2+(0.5*(pd-pu)*h)*h/3)/pp;#brazo vertical
            out[i,5]=pp*out[i,4];#momento de la fuerza horizontal
            out[i,6]=0.0;
            out[i,7]=kp_rankine(fi,0,c,gamma,h);
        else
            if c!=0
                err="Por ahora, la cohesión solo es permitida para el primer "
                err1="estrato, se ingnorará la cohesión en los demás estratos."
                print(err*err1);
            end
            kp=kp_rankine(fi,0);
            pp=0.5*gamma*kp*h^2;
            ppq=kp*h*qload;
            ppt=pp+ppq;
            out[i,1]=ppt;#fuerza horizontal
            out[i,3]=xu;#brazo horizontal
            out[i,4]=yd+(pp*h/3+ppq*h/2)/ppt;#brazo vertical
            out[i,5]=ppt*out[i,4];#momento de la fuerza horizontal
            out[i,6]=0.0;
            out[i,7]=kp;
        end
        qload+=gamma*h;
    end
    return out;
end

function uload_rankine_forces_rs(model::Wmodel{<:Real},uload::Real,alpha::Real)
    nel=size(model.pliners)[1];
    #declarando el objeto de salida
    #será una matriz de nelx6, por cada fila los datos serán
    #fuerza en la dirección horizontal, fuerza en la dirección vertical,
    #brazo horizontal, brazo vertical, Momento producido por la fuerza
    #horizontal y momento producido por la fuerza vertical.
    #Inicialmente la matriz será vacia (0x0)
    #los elementos se irán ingresando según se calculen los valores
    #correspondientes.
    out=VolatileArray(zeros(0,0));

    #declarando yd;
    yd=0.0;
    for i in 1:nel
        #coordenadas de los nudos
        n1=model.pliners[i,1];
        n2=model.pliners[i,2];
        xu=model.pnod[n1,1];
        yu=model.pnod[n1,2];
        err="Las coordenadas en `y` no son consistentes con el estrato anterior";
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
        if h<=0 || xu-xd>1e6
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
        ar=deg2rad(alpha);
        if i==1 && c!=0
            ka=ka_rankine(fi,alpha,c,gamma,h);
            pax=ka*h*uload;#horizontal
            pay=pax*tan(ar);#vertical
            out[i,1]=pax;#fuerza horizontal
            out[i,2]=pay;#fuerza vertical
        else
            if c!=0
                err="Por ahora, la cohesión solo es permitida para el primer "
                err1="estrato, se ingnorará esta en los demás estratos."
                print(err*err1);
            end
            ka=ka_rankine(fi,alpha);
            pax=ka*h*uload;#horizontal
            pay=pax*tan(ar);#vertical
            out[i,1]=pax;#fuerza horizontal
            out[i,2]=pay;#fuerza vertical
        end
        out[i,3]=xu;#brazo horizontal
        out[i,4]=yd+h/2;#brazo vertical
        out[i,5]=out[i,1]*out[i,4];#momento de la fuerza horizontal
        out[i,6]=out[i,2]*out[i,3];#momento de la fuerza vertical
    end
    return out;
end

"""
    check_stab_wt1(model::Wmodel{<:Real},wforces::VolatileArray{<:Real,2},
        rsf::VolatileArray{<:Real,2},lsf::VolatileArray{<:Real,2};
        arsf::VolatileArray{<:Real,2}=VolatileArray(zeros(0,6)),
        alsf::VolatileArray{<:Real,2}=VolatileArray(zeros(0,6)),k1::Real=2/3,
        k2::Real=2/3)
Chequeo de la estabilidad de un muro, para un muro típico, de preferencia
creado con `gravity_wall`, retornará un array de tamaño 5, con los siguientes
datos en ése orden: factor de seguridad contra el volteo, factor de seguridad
contra el deslizamiento, excentricidad, carga al pie del muro y carga en el
talón del muro.

    * `wforces`: Fuerzas generadas por el muro, creada con `wall_forces`.
    * `rsf`: Fuerzas generadas por el terreno a la derecha del muro, creada con
    `soil_rankine_forces_rs`.
    * `lsf`: Fuerzas generadas por el terreno a la izquierda del muro, creada
    con `soil_rankine_forces_ls`.
    * `arsf` y `alsf`: argumentos opcionales, Fuerzas adicionales análogas a
    `rsf` y `lsf`.
    *`k1` y `k2`: argumentos opcionales, fatores que afectarán al ángulo de
    fricción y la cohesión respectivamente para determinar la seguridad contra
    deslizamiento.
Las propiedades de suelo consideradas para calcular el factor de seguridad
contra el deslizamiento, se obtendrán del último estrato considerado de suelo
a la izquierda del muro.

"""
function check_stab_wt1(model::Wmodel{<:Real},wforces::VolatileArray{<:Real,2},
    rsf::VolatileArray{<:Real,2},lsf::VolatileArray{<:Real,2};
    arsf::VolatileArray{<:Real,2}=VolatileArray(zeros(0,6)),
    alsf::VolatileArray{<:Real,2}=VolatileArray(zeros(0,6)),k1::Real=2/3,
    k2::Real=2/3)
    #formación de salida
    out=VolatileArray(zeros(0,0));

    #*calculando factor de seguridad contra el volteo
    #wforces contiene en su quinta columna, lo momentos resistentes generados
    #por el peso del muro.
    Mr=sum(wforces[:,5]);
    #adicionalmente los momentos resistentes generados por el suelo, se
    #encuentran en la 6ta columna de rsf y 5ta columna de lsf
    Mr+=sum(rsf[:,6]);
    Mr+=sum(lsf[:,5]);
    Mr+=sum(arsf[:,6]);
    Mr+=sum(alsf[:,5]);
    #los momentos actuantes se encuetran en la 5ta columna de rsf y 6ta columna
    #de lsf
    Ma=sum(rsf[:,5]);
    Ma+=sum(lsf[:,6]);
    Ma+=sum(arsf[:,5]);
    Ma+=sum(alsf[:,6]);
    #escribiendo factor de seguridad contra el volteo
    out[1,1]=Mr/Ma;

    #*calculando factor de seguridad contra el deslizamiento
    #wforces contiene en su cuarta columna, las fuerzas verticales generadas
    #(peso del muro).
    vf=sum(wforces[:,4]);
    #adicionalmente las fuerzas verticales generadas por el empuje del suelo, se
    #encuentran en la 2da columna de rsf y de lsf
    vf+=sum(rsf[:,2]);
    vf+=sum(lsf[:,2]);
    vf+=sum(arsf[:,2]);
    vf+=sum(alsf[:,2]);
    #obteniendo las propiedas de suelo
    #obteniendo el último estrato del campo plinels
    id=model.plinels[end,3];
    fi=model.soilprop[id,1];
    c=model.soilprop[id,2];
    #modificando
    fi=deg2rad(k1*fi);
    c=k2*c;
    #el empuje pasivo (resistente), primera columna de lsf
    pp=sum(lsf[:,1]);
    pp+=sum(alsf[:,1]);
    #empuje activo (actuante), primera columna de rsf
    pa=sum(rsf[:,1]);
    pa+=sum(arsf[:,1]);
    #longitud de la base del muro, maximo valor de x, en la matriz de nudos
    B=maximum(model.nod[:,1]);
    #escribiendo factor de seguridad contra el deslizamiento
    out[1,2]=(vf*tan(fi)+B*c+pp)/pa;

    #excentricidad
    ex=B/2-(Mr-Ma)/vf;
    out[1,3]=ex;

    #qpie y q talón
    out[1,4]=vf*(1+6*ex/B)/B;
    out[1,5]=vf*(1-6*ex/B)/B;
    return out;
end

function combine_soil_forces(args::VolatileArray{<:Real,2}...)
    nel=length(args);
    #todos los argumentos deberán tener la misma altura y un mínimo de 6
    #columnas, en todas las columnas, excepto la 3 y la 4 que corresponderán a
    #los brazos, se aplicará la suma simple.

    #validando el primer argumento
    curr=args[1];
    wd=curr.width;
    if wd<6
        error("Se esperaban mínimo 6 columnas en todos los argumentos.")
    end
    out=+(args...);
    out[:,3]=out[:,6]./out[:,2];
    out[:,4]=out[:,5]./out[:,1];
    return VolatileArray(out);
end
