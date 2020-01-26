#revisión 0.0.0 26-01-2020, 00:50 Julia1.1.0
mutable struct VolatileArray{T,N}<:AbstractArray{T,N}
    arr::Array{T,1}
    height::Int64
    width::Int64

    function VolatileArray(arr::Array{T,1},
        height::Int64,width::Int64) where {T<:Real}
        if width*height!=length(arr)
            error("las dimensiones no son compatibles")
        else
            new{T,2}(arr,height,width)
        end
    end
end
function Base.size(x::VolatileArray{T,2}) where {T<:Real}
    return x.height, x.width;
end

function Base.getindex(x::VolatileArray{T,2},idy::Int64,
    idx::Int64) where {T<:Real}
    return x.arr[(idx-1)*x.height+idy];
end

function Base.getindex(x::VolatileArray{T,2},id::Int64) where {
    T<:Real}
    return x.arr[id];
end

function Base.setindex!(x::VolatileArray{T,2},value::N,
    idy::Int64, idx::Int64) where {T<:Real, N<:Real}

    h=x.height;
    w=x.width;
    if idx<=w && idy<=h
        @inbounds x.arr[(idx-1)*h+idy]=value;
    else
        #aumentando la altura
        if idy>h
            #+1 para guardar el elemento a eliminar antes de usar splice!
            len=idy-h+1;
            app=zeros(T,len);
            for i in 1:w
                idr=i*h+(i-1)*(len-1);
                @inbounds app[1]=x.arr[idr];
                @inbounds splice!(x.arr,idr,app);
            end
            h=idy;
            x.height=h;
        end
        #aumentando el ancho
        if idx>w
            @inbounds append!(x.arr,zeros(T,(idx-w)*h));
            w=idx;
            x.width=w;
        end
        @inbounds x.arr[(idx-1)*h+idy]=value;
    end
    return value;
end

function Base.setindex!(x::VolatileArray{T,2},value::N,
    id::Int64) where {T<:Real, N<:Real}
    if id<=length(x.arr)
        @inbounds x.arr[id]=value;
    else
        nw=Int64(ceil(id/x.height));
        append!(x.arr,zeros(T,(nw-x.width)*x.height));
        @inbounds x.arr[id]=value;
        x.width=nw;
    end
    return value;
end

"""
    push!(x::VolatileArray{T,2},n::Array{T,N}) where {T<:Real,N}
Inserta una fila o una columna al final de `x` dependiendo de las dimensiones
de `n`, `n` debe ser un vector fila o columna con dimensiones compatibles
con `size(x)`.
"""
function Base.push!(x::VolatileArray{T,2},n::Array{T,N}) where {T<:Real,N}
    dimy=size(x)[1];
    dimx=size(x)[2];

    ly=size(n)[1];
    lx=length(size(n))==1 ? 1 : size(n)[2];
    if ly==1 && lx==dimx
        #1 para guardar el elemento a eliminar antes de usar splice!
        #y 2 para el elemento a insertar
        len=2;
        app=zeros(T,2);
        for i in 1:dimx
            idr=i*dimy+(i-1);
            @inbounds app[1]=x.arr[idr];
            @inbounds app[2]=n[i];
            @inbounds splice!(x.arr,idr,app);
        end
        x.height+=1;
    elseif lx==1 && ly==dimy
        @inbounds append!(x.arr,n);
        x.width+=1;
    else
        error("las dimensiones no son compatibles")
    end

    return n;
end

"""
    append!(x::VolatileArray{T,2},n::Array{T,N}; dim::Int64=2) where {
        T<:Real,N}
Inserta filas o columnas al final de `x` dependiendo de las dimensiones
de `n`, `n` debe ser una matriz con dimensiones compatibles con `size(x)`, si
ambas dimensiones son compatibles se insertarán columnas a menos que `dim` sea
ingresado, `dim=1` agrega filas, `dim` solo se comprueba cuando ambas
dimensiones de `n` son compatibles.
"""
function Base.append!(x::VolatileArray{T,2},n::Array{T,N}; dim::Int64=2
    ) where {T<:Real,N}
    dimy=size(x)[1];
    dimx=size(x)[2];

    ly=size(n)[1];
    lx=length(size(n))==1 ? 1 : size(n)[2];
    if lx==dimx && (ly!=dimy || dim==1)
        #+1 para guardar el elemento a eliminar antes de usar splice!
        len=ly+1;
        app=zeros(T,len);
        for i in 1:dimx
            idr=i*dimy+(i-1)*ly;
            @inbounds app[1]=x.arr[idr];
            @inbounds app[2:len]=n[1:ly,i];
            @inbounds splice!(x.arr,idr,app);
        end
        x.height+=ly;
    elseif ly==dimy && (lx!=dimx || dim==2)
        @inbounds append!(x.arr,n);
        x.width+=lx;
    else
        error("argumnetos invalidos o dimensiones no compatibles")
    end

    return n;
end

"""
    insert!(x::VolatileArray{T,2},id::Int64, n::Array{T,N}, dim::Int64=2
        ) where {T<:Real,N}
Inserta filas o columnas en el lugar especificado por `id` en `x` dependiendo
de las dimensiones de `n`, `n` debe ser una matriz con dimensiones compatibles
con `size(x)`, si ambas dimensiones son compatibles se insertarán columnas
a menos que `dim` sea ingresado, `dim=1` agrega filas, `dim` solo se comprueba
cuando ambas dimensiones de `n` son compatibles.
"""
function Base.insert!(x::VolatileArray{T,2},id::Int64, n::Array{T,N};
    dim::Int64=2) where {T<:Real,N}
    dimy=size(x)[1];
    dimx=size(x)[2];

    ly=size(n)[1];
    lx=length(size(n))==1 ? 1 : size(n)[2];
    if lx==dimx && (ly!=dimy || dim==1)
        for i in 1:dimx
            idr=id+(i-1)*(dimy+ly);
            #se aumenta un cero que será reemplazado por una columna de n
            #al usar splice!
            @inbounds insert!(x.arr,idr,0);
            @inbounds splice!(x.arr,idr,n[:,i]);
        end
        x.height+=ly;
    elseif ly==dimy && (lx!=dimx || dim==2)
        idr=(id-1)*dimy+1;
        #se aumenta un cero que será reemplazado por n al usar splice!
        @inbounds insert!(x.arr,idr,0);
        @inbounds splice!(x.arr,idr,n);
        x.width+=lx;
    else
        error("argumentos invalidos o dimensiones no son compatibles")
    end

    return n;
end

"""
    deleteat!(x::VolatileArray{T,2},id::Int64; dim::Int64=2
        ) where {T<:Real,N}
Elimina una fila o columna del lugar especificado por `id` en `x`, se eliminará
la columna a menos que `dim` sea ingresado, `dim=1` elimina la fila.
"""
function Base.deleteat!(x::VolatileArray{T,2},id::Int64;
    dim::Int64=2) where {T<:Real,N}
    dimy=size(x)[1];
    dimx=size(x)[2];

    if dimy>=id && dim==1
        for i in 1:dimx
            idr=id +(i-1)*(dimy-1);
            @inbounds deleteat!(x.arr,idr);
        end
        x.height-=1;
    elseif dimx>=id && dim==2
        @inbounds deleteat!(x.arr,(id-1)*dimy+1:(id-1)*dimy+dimy);
        x.width-=1;
    else
        error("id no válido")
    end

    return x;
end

"""
    ka_rankine(fi::Real,alpha::Real)
Calcula el coeficiente de presión activa de Rankine para un
suelo granular `(c'=0)`, siendo:
*   `fi`: el ángulo de fricción por esfuerzo efectivo
     en grados sexagesimales.
*   `alpha`: el ángulo en grados sexagesimales que forma el
    terreno con la horizontal.

"""
function ka_rankine(fi::Real,alpha::Real)
    #validando argumentos
    if fi<0 || alpha<0
        error("los argumentos deben ser no negativos");
    end
    #convirtiendo los ángulos a radianes
    ar=deg2rad(alpha);
    fr=deg2rad(fi);
    #calculando los cosenos
    car=cos(ar);
    cfr=cos(fr);

    #ver Fundamentos de ingeniería de cimentaciones, Braja M.Das,
    #Cap. 7, numeral 7.3. Presión activa de tierra de Rankine
    return car*(car-sqrt(car^2-cfr^2))/(car+sqrt(car^2-cfr^2));
end

"""
    function ka_rankine(fi::Real,alpha::Real,c::Real,
        gamma::Real,z::Real)
Calcula el coeficiente de presión activa de Rankine, siendo:
*   `fi`: el ángulo de fricción por esfuerzo efectivo
     en grados sexagesimales.
*   `alpha`: el ángulo en grados sexagesimales que forma el
    terreno con la horizontal.
*   `c`: resistencia no drenada o cohesión aparente (KPa).
*   `gamma`: peso unitario del suelo (KN/m3).
*   `z`: profundidad a la que se calcula el coeficiente.

"""
function ka_rankine(fi::Real,alpha::Real,c::Real,
    gamma::Real,z::Real)
    #validando argumentos
    if fi<0 || alpha<0 || c<0 || gamma<=0 || z<=0
        err1="los argumentos deben ser no negativos,"
        err2=" el peso unitario y la profundidad"
        err3=" deben ser positivos"
        error(err1*err2*err3);
    end

    #convirtiendo los ángulos a radianes
    ar=deg2rad(alpha);
    fr=deg2rad(fi);

    #calculando los senos y cosenos
    car=cos(ar);
    cfr=cos(fr);
    sfr=sin(fr);

    #relación c/(gamma*z)
    rel=c/(gamma*z);

    #ver Fundamentos de ingeniería de cimentaciones, Braja M.Das,
    #Cap. 7, numeral 7.3. Presión activa de tierra de Rankine
    return ((2*car^2+2*rel*cfr*sfr-sqrt(4*car^2*(car^2-cfr^2)+
        4*rel^2*cfr^2+8*rel*car^2*sfr*cfr))/(cfr^2)-1)*car;
end

"""
    ka_coulomb(fi::T,delta::M,beta::N,alpha::O) where {
        T<:Real, M<:Real, N<:Real, O<:Real}
Calcula el coeficiente de presión activa de Coulomb, siendo:
*   `fi`: el ángulo de fricción por esfuerzo efectivo
     en grados sexagesimales.
*   `delta`: el ángulo de fricción entre el suelo y el muro.
*   `beta`: el ángulo de inclinación de la parte posterior
    del muro con la horizontal.
*   `alpha`: el ángulo de inclinación del
    terreno con la horizontal.

Todos los ángulos deben ser ingresados en grados sexagesimales.
"""
function ka_coulomb(fi::T,delta::M,beta::N,alpha::O) where {
    T<:Real, M<:Real, N<:Real, O<:Real}
    #convirtiendo los ángulos a radianes
    fr=deg2rad(fi);
    dr=deg2rad(delta);
    br=deg2rad(beta);
    ar=deg2rad(alpha);

    return ((sin(fr+br))^2)/ ((sin(br))^2*sin(br-dr)*
        (1+sqrt(sin(fr+dr)*sin(fr-ar)/(sin(br-dr)*
        sin(ar+br))))^2);
end

"""
    kr_maku(fi::T,OCR::N) where {T<:Real, N<:Real}
Calcula el coeficiente de presión en reposo de Mayne y Kulhawy,
 siendo:
*   `fi`: el ángulo de fricción por esfuerzo efectivo
     en grados sexagesimales.
*   `OCR`: relación de sobreconsolidación.
"""
function kr_maku(fi::T,OCR::N) where {T<:Real, N<:Real}
    #convirtiendo los ángulos a radianes
    fr=deg2rad(fi);

    #ver Fundamentos de ingeniería de cimentaciones, Braja M.Das,
    #Cap. 7, numeral 7.2. Presión lateral en reposo de tierra
    return (1-sin(fr))*OCR^(sin(fr));
end

"""
    kr_sch(OCR::T) where {T<:Real}
Calcula el coeficiente de presión en reposo de Schmertmann,
 siendo:
*   `OCR`: relación de sobreconsolidación.
"""
function kr_sch(OCR::T) where {T<:Real}
    return 0.5*OCR^0.5;
end

"""
"""
function build_wall(nodes::Array{T,2},
    elm::Array{N,2},elmp::Array{T,2},pbreak::N) where {
    T<:Real,N<:Integer}
    #número de elementos en cada matriz
    nel=size(elm)[1];

    #pbreak indica el punto donde terminan los elementos
    #triangulares
    for i in 1:pbreak
        #nudos (índices)
        @inbounds n1=elm[i,1];
        @inbounds n2=elm[i,2];
        @inbounds n3=elm[i,3];

        #nudos (coordenadas)
        @inbounds x1=nodes[n1,1];
        @inbounds y1=nodes[n1,2];
        @inbounds x2=nodes[n2,1];
        @inbounds y2=nodes[n2,2];
        @inbounds x3=nodes[n3,1];
        @inbounds y3=nodes[n3,2];

        #baricentro
        @inbounds elmp[i,2]=(x1+x2+x3)/3;
        @inbounds elmp[i,3]=(y1+y2+y3)/3;

        #área
        @inbounds elmp[i,1]=(x2*y3-y2*x3-x1*y3+y1*x3+
                        x1*y2-y1*x2)/2;
    end

    if pbreak<nel
        for i in pbreak+1:nel
            #nudos (índices)
            @inbounds n1=elm[i,1];
            @inbounds n2=elm[i,2];
            @inbounds n3=elm[i,3];
            @inbounds n4=elm[i,4];

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
            @inbounds elmp[i,1]=A1+A2;

            #baricentros
            @inbounds xm1=(x1+x2+x3)/3;
            @inbounds ym1=(y1+y2+y3)/3;
            @inbounds xm2=(x1+x3+x4)/3;
            @inbounds ym2=(y1+y3+y4)/3;
            @inbounds elmp[i,2]=(A1*xm1+A2*xm2)/elmp[i,1];
            @inbounds elmp[i,3]=(A1*ym1+A2*ym2)/elmp[i,1];
        end
    end
end
