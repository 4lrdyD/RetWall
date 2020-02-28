#revisión 0.0.6 27-02-2020, 23:10 Julia1.1.0
import LinearAlgebra: norm
export VolatileArray
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
            len=idy-h;
            app=zeros(T,len);
            for i in 1:w
                idr=i*h+(i-1)*len;
                @inbounds insert!(x.arr,idr+1,0);
                @inbounds splice!(x.arr,idr+1,app);
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
        for i in 1:dimx
            idr=i*dimy+i;
            @inbounds insert!(x.arr,idr,n[i]);
        end
        x.height+=1;
    elseif lx==1 && ly==dimy
        @inbounds append!(x.arr,n);
        x.width+=1;
    else
        error("las dimensiones no son compatibles")
    end

    return x;
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
        for i in 1:dimx
            idr=i*dimy+(i-1)*ly;
            @inbounds insert!(x.arr,idr+1,0)
            @inbounds splice!(x.arr,idr+1,n[1:ly,i]);
        end
        x.height+=ly;
    elseif ly==dimy && (lx!=dimx || dim==2)
        @inbounds append!(x.arr,n);
        x.width+=lx;
    else
        error("argumnetos invalidos o dimensiones no compatibles")
    end
    return x;
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
        error("argumentos invalidos o dimensiones no compatibles")
    end
    return x;
end

"""
    deleteat!(x::VolatileArray{T,2},id::Int64; dim::Int64
        ) where {T<:Real}
Elimina una fila o columna del lugar especificado por `id` en `x`, se eliminará
la columna si `dim=2`, `dim=1` elimina la fila.
"""
function Base.deleteat!(x::VolatileArray{T,2},id::Int64;
    dim::Int64) where {T<:Real}
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
        error("id o dimensión no válida")
    end
    return x;
end

"""
    deleteat!(x::VolatileArray{T,2},id::UnitRange{Int64}; dim::Int64=2
        ) where {T<:Real}
Elimina filas o columnas del rango especificado por `id` en `x`, se eliminarán
columnas a menos que `dim` sea ingresado, `dim=1` eliminará filas.
"""
function Base.deleteat!(x::VolatileArray{T,2},id::UnitRange{Int64};
    dim::Int64=2) where {T<:Real}
    dimy=size(x)[1];
    dimx=size(x)[2];

    fid=first(id);
    lid=last(id);
    nid=length(id);

    if dim==1 && dimy>=lid && lid>fid
        for i in 1:dimx
            fdr=fid +(i-1)*(dimy-nid);
            ldr=lid +(i-1)*(dimy-nid);
            @inbounds deleteat!(x.arr,fdr:ldr);
        end
        x.height-=nid;
    elseif dim==2 && dimx>=lid && lid>fid
        @inbounds deleteat!(x.arr,(fid-1)*dimy+1:lid*dimy);
        x.width-=nid;
    else
        error("id no válido")
    end
    return x;
end

"""
    Array(x::VolatileArray{T,2}) where {T<:Real}
Convierte un matriz del tipo `VolatileArray{T,2}` a una del tipo `Array{T,2}`,
la memoria es compartida.
"""
function Base.Array(x::VolatileArray{T,2}) where {T<:Real}
    #apuntador
    parr=pointer(x.arr);
    return unsafe_wrap(Array{T,2},parr,(x.height,x.width));
end

"""
    VolatileArray(x::Array{T,2}) where {T<:Real}
Convierte un matriz del tipo `Array{T,2}` a una del tipo `VolatileArray{T,2}`,
se crea una copia de los elementos de la matriz original.
"""
function VolatileArray(x::Array{T,2}) where {T<:Real}
    dims=size(x);
    narr=reshape(x,dims[1]*dims[2]);
    return VolatileArray(copy(narr),dims[1],dims[2]);
end

"""
    ver_orient_tri(nodes::Array{<:Real,2},elm::Array{<:Integer,2},
        idelm::Integer)
    ver_orient_tri(nodes::VolatileArray{<:Real,2},
        elm::VolatileArray{<:Integer,2}, idelm::Integer)
Verifica la orientación de un elemento triangular, devuelve 1 si el orden de
los nudos es antihorario, -1 cuando es horario y 0 cuando los nudos no forman
un triangulo.
*   `nodes` y `elm` son las matrices de nudos y de elementos según lo descrito
    en la ayuda de la función `build_wall`.
*   `idelm`: es la ubicación del elemento en `elm`.
"""
function ver_orient_tri(nodes::Array{<:Real,2},elm::Array{<:Integer,2},
    idelm::Integer)
    #nudos (índices)
    @inbounds n1=elm[idelm,1];
    @inbounds n2=elm[idelm,2];
    @inbounds n3=elm[idelm,3];
    return ver_orient_tri(nodes,elm,n1,n2,n3);
end

function ver_orient_tri(nodes::VolatileArray{<:Real,2},
    elm::VolatileArray{<:Integer,2}, idelm::Integer)
    #covirtiendo las matrices
    nnodes=Array(nodes);
    nelm=Array(elm);
    return ver_orient_tri(nnodes,nelm,idelm);
end

"""
    ver_orient_tri(nodes::Array{<:Real,2},elm::Array{<:Integer,2},
        id1::Integer,id2::Integer,id3::Integer)
    ver_orient_tri(nodes::VolatileArray{<:Real,2},
        elm::VolatileArray{<:Integer,2}, id1::Integer,id2::Integer,id3::Integer)
Verifica la orientación de un elemento triangular, devuelve 1 si el orden de
los nudos es antihorario, -1 cuando es horario y 0 cuando los nudos no forman
un triangulo.
*   `nodes` y `elm` son las matrices de nudos y de elementos según lo descrito
    en la ayuda de la función `build_wall`.
*   `id1`, `id2` e `id3` son los ids de los nudos (sus ubicaciones en `nodes`)
    que forman el triangulo.
"""
function ver_orient_tri(nodes::Array{<:Real,2},elm::Array{<:Integer,2},
    id1::Integer,id2::Integer,id3::Integer)
    #nudos (coordenadas)
    @inbounds x1=nodes[id1,1];
    @inbounds y1=nodes[id1,2];
    @inbounds x2=nodes[id2,1];
    @inbounds y2=nodes[id2,2];
    @inbounds x3=nodes[id3,1];
    @inbounds y3=nodes[id3,2];

    #traslación
    x3-=x1;
    y3-=y1;
    x2-=x1;
    y2-=y1;

    #Rotación:
    #calculando las funciones trigonométricas del ángulo que forma
    #el vector 1->2 con la horizontal
    #modulo
    mod=norm((x2,y2));
    s12=y2/mod;
    c12=x2/mod;
    #obteniendo valor final de y3, despues de la rotación
    y3=y3*c12-x3*s12;

    #y3>0 indica que el sentido de los nudos es antihorario
    #y3=0 indica que los nudos se encuentran en una misma recta
    #y3<0 indica que el sentido de los nudos es horario

    if y3>0
        return 1;
    elseif y3<0
        return -1;
    else
        return 0
    end
end

function ver_orient_tri(nodes::VolatileArray{<:Real,2},
    elm::VolatileArray{<:Integer,2}, id1::Integer,id2::Integer,id3::Integer)
    #convirtiendo las matrices
    nnodes=Array(nodes);
    nelm=Array(elm);
    return ver_orient_tri(nnodes,nelm,id1,id2,id3);
end

function orient_tri!(nodes::VolatileArray{<:Real,2},
    elm::VolatileArray{<:Integer,2}, idelm::Integer)
    key=ver_orient_tri(nodes,elm,idelm);
    if key==-1
        #orientación erronea
        @inbounds n2=elm[idelm,2];
        @inbounds n3=elm[idelm,3];
        @inbounds elm[idelm,2]=n3;
        @inbounds elm[idelm,3]=n2;
    elseif key==0
        #no forma un triangulo por tanto se elimina el elemento
        deleteat!(elm,idelm,dim=1);
    else
        #para key=1 la orientación es correcta y no requiere ningun cambio
    end
    return elm;
end

"""
    ver_orient_quad(nodes::Array{<:Real,2},elm::Array{<:Integer,2},
        idelm::Integer)
    ver_orient_quad(nodes::VolatileArray{<:Real,2},
        elm::VolatileArray{<:Integer,2}, idelm::Integer)
Verifica la orientación de un elemento cuadrangular, devuelve `key1*10+key2`
siendo `key1` y `key2` los valores devueltos por la función `ver_orient_tri`,
para los subtriangulos que componen el cuadrilátero (`id1-id2-id3` e
`id1-id3-id4`).
*   `nodes` y `elm` son las matrices de nudos y de elementos según lo descrito
    en la ayuda de la función `build_wall`.
*   `idelm`: es la ubicación del elemento en `elm`.
"""
function ver_orient_quad(nodes::Array{<:Real,2},elm::Array{<:Integer,2},
    idelm::Integer)
    #nudos (índices)
    @inbounds n1=elm[idelm,1];
    @inbounds n2=elm[idelm,2];
    @inbounds n3=elm[idelm,3];
    @inbounds n4=elm[idelm,4];
    return ver_orient_quad(nodes,elm,n1,n2,n3,n4);
end

function ver_orient_quad(nodes::VolatileArray{<:Real,2},
    elm::VolatileArray{<:Integer,2}, idelm::Integer)
    #covirtiendo las matrices
    nnodes=Array(nodes);
    nelm=Array(elm);
    return ver_orient_quad(nnodes,nelm,idelm);
end

"""
    ver_orient_quad(nodes::Array{<:Real,2},elm::Array{<:Integer,2},
        id1::Integer,id2::Integer,id3::Integer,id4::Integer)
    ver_orient_quad(nodes::VolatileArray{<:Real,2},
        elm::VolatileArray{<:Integer,2},id1::Integer,id2::Integer,id3::Integer,
        id4::Integer)
Verifica la orientación de un elemento cuadrangular, devuelve `key1*10+key2`
siendo `key1` y `key2` los valores devueltos por la función `ver_orient_tri`,
para los subtriangulos que componen el cuadrilátero (`id1-id2-id3` e
`id1-id3-id4`).

*   `nodes` y `elm` son las matrices de nudos y de elementos según lo descrito
    en la ayuda de la función `build_wall`.
*   `id1`, `id2`, `id3` e `id4` son los ids de los nudos (sus ubicaciones
    en `nodes`) que forman el cuadrilátero.
"""
function ver_orient_quad(nodes::Array{<:Real,2},elm::Array{<:Integer,2},
    id1::Integer,id2::Integer,id3::Integer,id4::Integer)
    #orientación de los triangulos
    key1=ver_orient_tri(nodes,elm,id1,id2,id3);
    key2=ver_orient_tri(nodes,elm,id1,id3,id4);
    return key1*10+key2;
end

function ver_orient_quad(nodes::VolatileArray{<:Real,2},
    elm::VolatileArray{<:Integer,2},id1::Integer,id2::Integer,id3::Integer,
    id4::Integer)
    nnodes=Array(nodes);
    nelm=Array(elm);
    return ver_orient_quad(nnodes,nelm,id1,id2,id3,id4);
end

function orient_quad!(nodes::VolatileArray{<:Real,2},
    elm::VolatileArray{<:Integer,2}, idelm::Integer,pbreak::Integer)
    #*   pbreak es la  ubicación en `elm`, del último elemento correspondiente a
    #    elementos triangulares, será necesario cuando tengamos que agregar un
    #    triángulo a causa del cuadrilátero mal formado.
    #obteniendo la llave
    key=ver_orient_quad(nodes,elm,idelm);
    #* key=11, significa que el cuadrilátero esta bien formado y no se necesitan
    #operaciones adicionales.
    #* key=10, significa que el primer triángulo está bien formado pero el
    #segundo triángulo no se forma.
    #* key=9, significa que el primer triángulo está bien formado y el segundo
    #mal formado (nudos en sentido horario)
    #* key=1, significa que el primer triángulo no se forma y el segundo está
    #bien formado.
    #* key=0, significa que ningún triángulo se forma.
    #*key=-1, significa que el primer triángulo no se forma y el segundo está
    #mal formado.
    #*key=-9, significa que el primer triángulo está mal formado y el primer
    #triángulo está bien formado.
    #*key=-10, significa que el primer triángulo está mal formado y el segundo
    #triángulo no se forma.
    #*key=-11, significa que ambos triángulos están mal formados.

    #las soluciones para los valores de key diferentes de 9 y -9 son directas.

    #la función devolverá el valor de out que dependerá si el elemento fue
    #eliminado (out=-1), si se traslado al grupo de triángulos (out=1), o si no
    #se realizó ningún cambio (out=0).
    out=0;

    #valor que nos servirá solo para intentar corregir key=9
    vhlp=1;

    #111 será el valor de salida del bucle
    while key!=111
        if key==11
            key=111;#no requiere cambios
        elseif key==10#solo se conservará el primer triángulo
            @inbounds myrow=elm[idelm:idelm,:];
            #cambiando myrow[4]=0, indicamos que se trata de un triángulo
            #y ya no de un cuadrilátero.
            @inbounds myrow[4]=0;
            deleteat!(elm,idelm,dim=1);
            insert!(elm,pbreak+1,myrow,dim=1);
            out=1;
            key=111;
        elseif key==9
            #se intentará corregir 3 veces para valores de vhlp=1,2 y 3
            if vhlp==1
                #se intercambiará 3 y 4
                @inbounds temp=elm[idelm,3];
                @inbounds elm[idelm,3]=elm[idelm,4];
                @inbounds elm[idelm,4]=temp;
                key=ver_orient_quad(nodes,elm,idelm);
                vhlp+=1;
            elseif vhlp==2
                #se rotará 124 a 412
                @inbounds temp=elm[idelm,1];
                @inbounds elm[idelm,1]=elm[idelm,4];
                @inbounds elm[idelm,4]=elm[idelm,2];
                @inbounds elm[idelm,2]=temp;
                key=ver_orient_quad(nodes,elm,idelm);
                vhlp+=1;
            elseif vhlp==3
                #se rotará 123 a 312
                @inbounds temp=elm[idelm,1];
                @inbounds elm[idelm,1]=elm[idelm,3];
                @inbounds elm[idelm,3]=elm[idelm,2];
                @inbounds elm[idelm,2]=temp;
                key=ver_orient_quad(nodes,elm,idelm);
                vhlp+=1;
            else
                error("no se pudo corregir el elemento $idelm")
            end
        elseif key==1#solo se conservará el segundo triángulo
            @inbounds myrow=elm[idelm:idelm,:];
            #trasladando los nudos del triángulo válido
            @inbounds myrow[2]=myrow[3];
            @inbounds myrow[3]=myrow[4];
            #cambiando myrow[4]=0, indicamos que se trata de un triángulo
            #y ya no de un cuadrilátero.
            @inbounds myrow[4]=0;
            deleteat!(elm,idelm,dim=1);
            insert!(elm,pbreak+1,myrow,dim=1);
            out=1;
            key=111;
        elseif key==0#no se forma ningún cuadrilátero entonces eliminar
            deleteat!(elm,idelm,dim=1);
            key=111;
        elseif key==-1#solo se conservará el segundo triángulo pero
                      #reorientándolo
            @inbounds myrow=elm[idelm:idelm,:];
            #trasladando y reorientando los nudos del triángulo válido
            @inbounds myrow[2]=myrow[4];
            #cambiando myrow[4]=0, indicamos que se trata de un triángulo
            #y ya no de un cuadrilátero.
            @inbounds myrow[4]=0;
            deleteat!(elm,idelm,dim=1);
            insert!(elm,pbreak+1,myrow,dim=1);
            out=1;
            key=111;
        elseif key==-9
            #se intercambiarán los nudos 2 y 4 para obtener key==9
            @inbounds temp=elm[idelm,2];
            @inbounds elm[idelm,2]=elm[idelm,4];
            @inbounds elm[idelm,4]=temp;
            key=9;
        elseif key==-10#solo se conservará el primer triángulo pero
                      #reorientándolo
            @inbounds myrow=elm[idelm:idelm,:];
            #reorientando los nudos del triángulo válido
            @inbounds myrow[4]=myrow[2];#temporal
            @inbounds myrow[2]=myrow[3];
            @inbounds myrow[3]=myrow[4];
            #cambiando myrow[4]=0, indicamos que se trata de un triángulo
            #y ya no de un cuadrilátero.
            @inbounds myrow[4]=0;
            deleteat!(elm,idelm,dim=1);
            insert!(elm,pbreak+1,myrow,dim=1);
            out=1;
            key=111;
        elseif key==-11
            #se intercambiarán los nudos 2 y 4 para obtener key==11
            @inbounds temp=elm[idelm,2];
            @inbounds elm[idelm,2]=elm[idelm,4];
            @inbounds elm[idelm,4]=temp;
            key=111;
        else
            error("key no valido")
        end
    end
    return out;
end
