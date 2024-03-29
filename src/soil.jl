#revisión 0.0.4 16-02-2024, 23:14 Julia1.9.2
"""
    ka_rankine(fi::Real,alpha::Real)
Calcula el coeficiente de presión activa de Rankine para un
suelo granular `(c'=0)` y pared vertical, siendo:
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
    ka_rankine(fi::Real,alpha::Real,c::Real,gamma::Real,z::Real)
Calcula el coeficiente de presión activa de Rankine (pared vertical), siendo:
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
    ka_coulomb(fi::Real,delta::Real,beta::Real,alpha::Real)
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
function ka_coulomb(fi::Real,delta::Real,beta::Real,alpha::Real)
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
    ka_dynamic_coulomb(fi::Real,delta::Real,beta::Real,alpha::Real,theta::Real)
Calcula el coeficiente de presión activa dinámica de Coulomb, siendo:
*   `fi`: el ángulo de fricción por esfuerzo efectivo
     en grados sexagesimales.
*   `delta`: el ángulo de fricción entre el suelo y el muro.
*   `beta`: el ángulo de inclinación de la parte posterior
    del muro con la horizontal.
*   `alpha`: el ángulo de inclinación del
    terreno con la horizontal.
*   `theta`: Ángulo que depende de los coeficientes sísmicos según: `theta=atan(kh/(1-Kv))`

Todos los ángulos deben ser ingresados en grados sexagesimales.
"""
function ka_dynamic_coulomb(fi::Real,delta::Real,beta::Real,alpha::Real,theta::Real)
    #convirtiendo los ángulos
    return ((sind(fi+beta-theta))^2)/ ((cosd(theta))*(sind(beta))^2*sind(beta-theta-delta)*
        (1+sqrt(sind(fi+delta)*sind(fi-theta-alpha)/(sind(beta-delta-theta)*
        sind(alpha+beta))))^2);
end

"""
    kr_maku(fi::Real,OCR::Real)
Calcula el coeficiente de presión en reposo de Mayne y Kulhawy,
 siendo:
*   `fi`: el ángulo de fricción por esfuerzo efectivo
     en grados sexagesimales.
*   `OCR`: relación de sobreconsolidación.
"""
function kr_maku(fi::Real,OCR::Real)
    #convirtiendo los ángulos a radianes
    fr=deg2rad(fi);

    #ver Fundamentos de ingeniería de cimentaciones, Braja M.Das,
    #Cap. 7, numeral 7.2. Presión lateral en reposo de tierra
    return (1-sin(fr))*OCR^(sin(fr));
end

"""
    kr_sch(OCR::Real)
Calcula el coeficiente de presión en reposo de Schmertmann,
 siendo:
*   `OCR`: relación de sobreconsolidación.
"""
kr_sch(OCR::Real)=0.5*OCR^0.5;

"""
    kp_rankine(fi::Real,alpha::Real)
Calcula el coeficiente de presión pasiva de Rankine para un
suelo granular `(c'=0)` y pared vertical, siendo:
*   `fi`: el ángulo de fricción por esfuerzo efectivo
     en grados sexagesimales.
*   `alpha`: el ángulo en grados sexagesimales que forma el
    terreno con la horizontal.

"""
function kp_rankine(fi::Real,alpha::Real)
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
    #Cap. 7, numeral 7.11. Presión pasiva de tierra de Rankine:
    #Cara posterior vertical y relleno inclinado
    return car*(car+sqrt(car^2-cfr^2))/(car-sqrt(car^2-cfr^2));
end

"""
    kp_rankine(fi::Real,alpha::Real,c::Real,gamma::Real,z::Real)
Calcula el coeficiente de presión pasiva de Rankine (pared vertical), siendo:
*   `fi`: el ángulo de fricción por esfuerzo efectivo
     en grados sexagesimales.
*   `alpha`: el ángulo en grados sexagesimales que forma el
    terreno con la horizontal.
*   `c`: resistencia no drenada o cohesión aparente (KPa).
*   `gamma`: peso unitario del suelo (KN/m3).
*   `z`: profundidad a la que se calcula el coeficiente.

"""
function kp_rankine(fi::Real,alpha::Real,c::Real,
    gamma::Real,z::Real)
    #validando argumentos
    if fi<0 || alpha<0 || c<0 || gamma<=0 #|| z<=0
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
    #Cap. 7, numeral 7.11. Presión pasiva de tierra de Rankine:
    #Cara posterior vertical y relleno inclinado
    return ((2*car^2+2*rel*cfr*sfr+sqrt(4*car^2*(car^2-cfr^2)+
        4*rel^2*cfr^2+8*rel*car^2*sfr*cfr))/(cfr^2)-1)*car;
end

"""
    controlled_Kh(Aa::Real,Av::Real,d::Real)
Calcula el coeficiente sísmico de aceleración horizontal `Kh` para un análisis por
desplazamientos controlados:
*   `Aa`: Coeficiente que representa la aceleración horizontal pico efectiva.
*   `Av`: Coeficiente de aceleración que representa la velocidad horizontal pico
    efectiva.
*   `d`: el desplazamiento tolerable en pulgadas.

"""
function controlled_Kh(Aa::Real,Av::Real,d::Real)
    #validando argumentos
    if Aa<=0 || Av<=0 || d<=0
        err1="los argumentos deben ser positivos"
        error(err1);
    end
    return Aa*(.2*Av^2/(Aa*d))^.25;
end

"""
    seismic_angle(Kh::Real,Kv::Real)
Calcula el ángulo sísmico de inercia.
*   `Kh`: coeficiente sísmico de aceleración horizontal.
*   `Kv`: coeficiente sísmico de aceleración vertical.

"""
function seismic_angle(Kh::Real,Kv::Real)
    #validando argumentos
    if Kh<0 || Kv<0
        err1="los argumentos deben ser no negativos"
        error(err1);
    end
    return atand(Kh/(1-Kv));
end
