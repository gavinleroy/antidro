class t_P_2 {constructor() {this.run = this.run.bind(this);}run(x,
               y) {this.x = x;
this.y = y;return ({ return: x + y });
}}
class t_div_5 {constructor() {this.run = this.run.bind(this);}run(e1,
                 e2) {this.e1 = e1;
this.e2 = e2;return ((() => { let d = document.createElement('div');
d.appendChild(e1); d.appendChild(e2); return { return: d,
updater_result_children_off_0: (e) => { d.replaceChild(e, d.childNodes[0]);
}, updater_result_children_off_1: (e) => { d.replaceChild(e,
d.childNodes[1]); } }; })());
}}
class t_div_6 {constructor() {this.run = this.run.bind(this);}run(n) {
                 this.n = n;return ((() => { let d =
                   document.createElement('div'); d.innerHTML = n; return {
                   return: d, updater_result_children_off_0: (n) => {
                   d.innerHTML = n; } }; })());}}
class t_button_7 {constructor() {this.run = this.run.bind(this);}run(s,
                    onclick) {this.s = s;
this.onclick = onclick;return ((() => { let b =
document.createElement('button');
          b.appendChild(document.createTextNode(s));
          b.onclick = onclick;
          return { return: b, updater_result_children_off_0: (s) => {
b.replaceChild( document.createTextNode(s), b.childNodes[0])
            } }; })());
}}
class t_render_into_8 {constructor() {this.run = this.run.bind(this);}run(id,
                         component) {this.id = id;
this.component = component;return ((() => { window.onload = () => {let root =
document.getElementById(id);
            root.appendChild(component);
          }; })());
}}
class t_gensym_39 {constructor(t_counter_32,
                     updater_t_counter_32_value) {this.t_counter_32 = t_counter_32;
                                                    
this.updater_t_counter_32_value = updater_t_counter_32_value;
this.run = this.run.bind(this);}run() {let t_gensym_33 = new t_P_2();
                                         let updater_t_gensym_33 = () => {
                                         let t_gensym_67 = new t_P_2();
                                           this.t_gensym_33 = t_gensym_67;
                                           return (void(0));};
                                         let t_gensym_34 = 1;
                                         let updater_t_gensym_34 = () => {
                                         let t_gensym_66 = 1;
                                           this.t_gensym_34 = t_gensym_66;
                                           return (void(0));};
                                         let t_gensym_35 = this.t_counter_32.value;
                                         
                                         let updater_t_gensym_35 = () => {
                                         let t_gensym_65 = this.t_counter_32.value;
                                            this.t_gensym_35 = t_gensym_65;
                                           return (void(0));};
                                         let t_gensym_36 = this.t_gensym_33.run(t_gensym_34,
                                         t_gensym_35);
                                         let updater_t_gensym_36 = () => {
                                         let t_gensym_64 = this.t_gensym_33.run(t_gensym_34,
                                           t_gensym_35);
                                           this.t_gensym_36 = t_gensym_64;
                                           return (void(0));};
                                         let t_gensym_37 = this.t_gensym_36.return;
                                         
                                         let updater_t_gensym_37 = () => {
                                         let t_gensym_63 = this.t_gensym_36.return;
                                            this.t_gensym_37 = t_gensym_63;
                                           return (void(0));};
                                         this.t_counter_32.value = t_gensym_37;
                                         
                                         let updater_t_gensym_38 = () => {
                                         this.t_counter_32.value = t_gensym_37;
                                            this.t_gensym_38 = t_gensym_62;
                                           return (void(0));};
                                         return ({return: t_gensym_38});}}
class t_gensym_85 {constructor(t_counter_32,
                     updater_t_counter_32_value) {this.t_counter_32 = t_counter_32;
                                                    
this.updater_t_counter_32_value = updater_t_counter_32_value;
this.run = this.run.bind(this);}run() {let t_gensym_33 = new t_P_2();
                                         let updater_t_gensym_33 = () => {
                                         let t_gensym_67 = new t_P_2();
                                           this.t_gensym_33 = t_gensym_67;
                                           return (void(0));};
                                         let t_gensym_34 = 1;
                                         let updater_t_gensym_34 = () => {
                                         let t_gensym_66 = 1;
                                           this.t_gensym_34 = t_gensym_66;
                                           return (void(0));};
                                         let t_gensym_35 = this.t_counter_32.value;
                                         
                                         let updater_t_gensym_35 = () => {
                                         let t_gensym_65 = this.t_counter_32.value;
                                            this.t_gensym_35 = t_gensym_65;
                                           return (void(0));};
                                         let t_gensym_36 = this.t_gensym_33.run(t_gensym_34,
                                         t_gensym_35);
                                         let updater_t_gensym_36 = () => {
                                         let t_gensym_64 = this.t_gensym_33.run(t_gensym_34,
                                           t_gensym_35);
                                           this.t_gensym_36 = t_gensym_64;
                                           return (void(0));};
                                         let t_gensym_37 = this.t_gensym_36.return;
                                         
                                         let updater_t_gensym_37 = () => {
                                         let t_gensym_63 = this.t_gensym_36.return;
                                            this.t_gensym_37 = t_gensym_63;
                                           return (void(0));};
                                         this.t_counter_32.value = t_gensym_37;
                                         
                                         let updater_t_gensym_38 = () => {
                                         this.t_counter_32.value = t_gensym_37;
                                            this.t_gensym_38 = t_gensym_62;
                                           return (void(0));};
                                         return ({return: t_gensym_38});}}
class t_App_54 {constructor() {this.run = this.run.bind(this);}run() {
                  let t_gensym_30 = 0;
                    let updater_t_gensym_30 = () => {let t_gensym_88 = 0;
                                                       this.t_gensym_30 = t_gensym_88;
                                                        return (void(0));};
                    let t_gensym_31 = {value: t_gensym_30};
                    let updater_t_gensym_31 = () => {let t_gensym_87 = {
                                                       value: t_gensym_30};
                                                       this.t_gensym_31 = t_gensym_87;
                                                        return (void(0));};
                    let updater_t_gensym_31_value = () => {this.t_gensym_31.value = t_gensym_30;
                                                             
                                                             return (void(0));};
                     let t_counter_32 = this.t_gensym_31;
                    let updater_t_counter_32 = () => {let t_gensym_86 = this.t_gensym_31;
                                                        
                                                        this.t_counter_32 = t_gensym_86;
                                                         return (void(0));};
                    let updater_t_gensym_39 = () => {this.t_gensym_39 = t_gensym_85;
                                                        return (void(0));};
                    let t_incr_40 = new t_gensym_39(t_counter_32,
                    updater_t_counter_32_value);
                    let updater_t_incr_40 = () => {let t_gensym_84 = new t_gensym_39(t_counter_32,
                                                     updater_t_counter_32_value);
                                                     
                                                     this.t_incr_40 = t_gensym_84;
                                                      return (void(0));};
                    let t_gensym_41 = this.t_div_14;
                    let updater_t_gensym_41 = () => {let t_gensym_83 = this.t_div_14;
                                                       
                                                       this.t_gensym_41 = t_gensym_83;
                                                        return (void(0));};
                    let t_gensym_42 = this.t_button_22;
                    let updater_t_gensym_42 = () => {let t_gensym_82 = this.t_button_22;
                                                       
                                                       this.t_gensym_42 = t_gensym_82;
                                                        return (void(0));};
                    let t_gensym_43 = "Click";
                    let updater_t_gensym_43 = () => {let t_gensym_81 = "Click";
                                                       
                                                       this.t_gensym_43 = t_gensym_81;
                                                        return (void(0));};
                    let t_gensym_44 = new t_incr_40(t_counter_32,
                    updater_t_counter_32_value);
                    let updater_t_gensym_44 = () => {let t_gensym_80 = new t_incr_40(t_counter_32,
                                                       updater_t_counter_32_value);
                                                       
                                                       this.t_gensym_44 = t_gensym_80;
                                                        return (void(0));};
                    let t_gensym_45 = this.t_button_7.run(t_gensym_43,
                    t_gensym_44);
                    let updater_t_gensym_45 = () => {let t_gensym_79 = this.t_button_7.run(t_gensym_43,
                                                       t_gensym_44);
                                                       this.t_gensym_45 = t_gensym_79;
                                                        return (void(0));};
                    let t_gensym_46 = this.t_gensym_45.return;
                    let updater_t_gensym_46 = () => {let t_gensym_78 = this.t_gensym_45.return;
                                                       
                                                       this.t_gensym_46 = t_gensym_78;
                                                        return (void(0));};
                    let updater_t_gensym_46_children_off_0 = this.t_gensym_45.updater_result_children_off_0;
                    
                    let updater_updater_t_gensym_46_children_off_0 = () => {
                    let t_gensym_77 = this.t_gensym_45.updater_result_children_off_0;
                       this.updater_t_gensym_46_children_off_0 = t_gensym_77;
                      return (void(0));}; let t_gensym_47 = this.t_div_14;
                    let updater_t_gensym_47 = () => {let t_gensym_76 = this.t_div_14;
                                                       
                                                       this.t_gensym_47 = t_gensym_76;
                                                        return (void(0));};
                    let t_gensym_48 = this.t_counter_32.value;
                    let updater_t_gensym_48 = () => {let t_gensym_75 = this.t_counter_32.value;
                                                       
                                                       this.t_gensym_48 = t_gensym_75;
                                                        return (void(0));};
                    let t_gensym_49 = this.t_div_6.run(t_gensym_48);
                    let updater_t_gensym_49 = () => {let t_gensym_74 = this.t_div_6.run(t_gensym_48);
                                                       
                                                       this.t_gensym_49 = t_gensym_74;
                                                        return (void(0));};
                    let t_gensym_50 = this.t_gensym_49.return;
                    let updater_t_gensym_50 = () => {let t_gensym_73 = this.t_gensym_49.return;
                                                       
                                                       this.t_gensym_50 = t_gensym_73;
                                                        return (void(0));};
                    let updater_t_gensym_50_children_off_0 = this.t_gensym_49.updater_result_children_off_0;
                    
                    let updater_updater_t_gensym_50_children_off_0 = () => {
                    let t_gensym_72 = this.t_gensym_49.updater_result_children_off_0;
                       this.updater_t_gensym_50_children_off_0 = t_gensym_72;
                      return (void(0));};
                    let t_gensym_51 = this.t_div_5.run(t_gensym_46,
                    t_gensym_50);
                    let updater_t_gensym_51 = () => {let t_gensym_71 = this.t_div_5.run(t_gensym_46,
                                                       t_gensym_50);
                                                       this.t_gensym_51 = t_gensym_71;
                                                        return (void(0));};
                    let t_gensym_52 = this.t_gensym_51.return;
                    let updater_t_gensym_52 = () => {let t_gensym_70 = this.t_gensym_51.return;
                                                       
                                                       this.t_gensym_52 = t_gensym_70;
                                                        return (void(0));};
                    let updater_t_gensym_52_children_off_0 = this.t_gensym_51.updater_result_children_off_0;
                    
                    let updater_updater_t_gensym_52_children_off_0 = () => {
                    let t_gensym_69 = this.t_gensym_51.updater_result_children_off_0;
                       this.updater_t_gensym_52_children_off_0 = t_gensym_69;
                      return (void(0));};
                    let updater_t_gensym_52_children_off_1 = this.t_gensym_51.updater_result_children_off_1;
                    
                    let updater_updater_t_gensym_52_children_off_1 = () => {
                    let t_gensym_68 = this.t_gensym_51.updater_result_children_off_1;
                       this.updater_t_gensym_52_children_off_1 = t_gensym_68;
                      return (void(0));}; return ({return: t_gensym_52});}}
class t_gensym_39 {constructor(t_counter_32,
                     updater_t_counter_32_value) {this.t_counter_32 = t_counter_32;
                                                    
this.updater_t_counter_32_value = updater_t_counter_32_value;
this.run = this.run.bind(this);}run() {let t_gensym_33 = new t_P_2();
                                         let updater_t_gensym_33 = () => {
                                         let t_gensym_67 = new t_P_2();
                                           this.t_gensym_33 = t_gensym_67;
                                           return (void(0));};
                                         let t_gensym_34 = 1;
                                         let updater_t_gensym_34 = () => {
                                         let t_gensym_66 = 1;
                                           this.t_gensym_34 = t_gensym_66;
                                           return (void(0));};
                                         let t_gensym_35 = this.t_counter_32.value;
                                         
                                         let updater_t_gensym_35 = () => {
                                         let t_gensym_65 = this.t_counter_32.value;
                                            this.t_gensym_35 = t_gensym_65;
                                           return (void(0));};
                                         let t_gensym_36 = this.t_gensym_33.run(t_gensym_34,
                                         t_gensym_35);
                                         let updater_t_gensym_36 = () => {
                                         let t_gensym_64 = this.t_gensym_33.run(t_gensym_34,
                                           t_gensym_35);
                                           this.t_gensym_36 = t_gensym_64;
                                           return (void(0));};
                                         let t_gensym_37 = this.t_gensym_36.return;
                                         
                                         let updater_t_gensym_37 = () => {
                                         let t_gensym_63 = this.t_gensym_36.return;
                                            this.t_gensym_37 = t_gensym_63;
                                           return (void(0));};
                                         this.t_counter_32.value = t_gensym_37;
                                         
                                         let updater_t_gensym_38 = () => {
                                         this.t_counter_32.value = t_gensym_37;
                                            this.t_gensym_38 = t_gensym_62;
                                           return (void(0));};
                                         return ({return: t_gensym_38});}}
class t_gensym_85 {constructor(t_counter_32,
                     updater_t_counter_32_value) {this.t_counter_32 = t_counter_32;
                                                    
this.updater_t_counter_32_value = updater_t_counter_32_value;
this.run = this.run.bind(this);}run() {let t_gensym_33 = new t_P_2();
                                         let updater_t_gensym_33 = () => {
                                         let t_gensym_67 = new t_P_2();
                                           this.t_gensym_33 = t_gensym_67;
                                           return (void(0));};
                                         let t_gensym_34 = 1;
                                         let updater_t_gensym_34 = () => {
                                         let t_gensym_66 = 1;
                                           this.t_gensym_34 = t_gensym_66;
                                           return (void(0));};
                                         let t_gensym_35 = this.t_counter_32.value;
                                         
                                         let updater_t_gensym_35 = () => {
                                         let t_gensym_65 = this.t_counter_32.value;
                                            this.t_gensym_35 = t_gensym_65;
                                           return (void(0));};
                                         let t_gensym_36 = this.t_gensym_33.run(t_gensym_34,
                                         t_gensym_35);
                                         let updater_t_gensym_36 = () => {
                                         let t_gensym_64 = this.t_gensym_33.run(t_gensym_34,
                                           t_gensym_35);
                                           this.t_gensym_36 = t_gensym_64;
                                           return (void(0));};
                                         let t_gensym_37 = this.t_gensym_36.return;
                                         
                                         let updater_t_gensym_37 = () => {
                                         let t_gensym_63 = this.t_gensym_36.return;
                                            this.t_gensym_37 = t_gensym_63;
                                           return (void(0));};
                                         this.t_counter_32.value = t_gensym_37;
                                         
                                         let updater_t_gensym_38 = () => {
                                         this.t_counter_32.value = t_gensym_37;
                                            this.t_gensym_38 = t_gensym_62;
                                           return (void(0));};
                                         return ({return: t_gensym_38});}}
class t_gensym_96 {constructor() {this.run = this.run.bind(this);}run() {
                     let t_gensym_30 = 0;
                       let updater_t_gensym_30 = () => {let t_gensym_88 = 0;
                                                          this.t_gensym_30 = t_gensym_88;
                                                           return (void(0));};
                        let t_gensym_31 = {value: t_gensym_30};
                       let updater_t_gensym_31 = () => {let t_gensym_87 = {
                                                          value: t_gensym_30};
                                                          
                                                          this.t_gensym_31 = t_gensym_87;
                                                           return (void(0));};
                       
                       let updater_t_gensym_31_value = () => {this.t_gensym_31.value = t_gensym_30;
                                                                
                                                                return (void(0));
                                                                };
                       let t_counter_32 = this.t_gensym_31;
                       let updater_t_counter_32 = () => {let t_gensym_86 = this.t_gensym_31;
                                                           
                                                           this.t_counter_32 = t_gensym_86;
                                                            return (void(0));};
                       
                       let updater_t_gensym_39 = () => {this.t_gensym_39 = t_gensym_85;
                                                           return (void(0));};
                        let t_incr_40 = new t_gensym_39(t_counter_32,
                       updater_t_counter_32_value);
                       let updater_t_incr_40 = () => {let t_gensym_84 = new t_gensym_39(t_counter_32,
                                                        updater_t_counter_32_value);
                                                        
                                                        this.t_incr_40 = t_gensym_84;
                                                         return (void(0));};
                       let t_gensym_41 = this.t_div_14;
                       let updater_t_gensym_41 = () => {let t_gensym_83 = this.t_div_14;
                                                          
                                                          this.t_gensym_41 = t_gensym_83;
                                                           return (void(0));};
                        let t_gensym_42 = this.t_button_22;
                       let updater_t_gensym_42 = () => {let t_gensym_82 = this.t_button_22;
                                                          
                                                          this.t_gensym_42 = t_gensym_82;
                                                           return (void(0));};
                        let t_gensym_43 = "Click";
                       let updater_t_gensym_43 = () => {let t_gensym_81 = "Click";
                                                          
                                                          this.t_gensym_43 = t_gensym_81;
                                                           return (void(0));};
                        let t_gensym_44 = new t_incr_40(t_counter_32,
                       updater_t_counter_32_value);
                       let updater_t_gensym_44 = () => {let t_gensym_80 = new t_incr_40(t_counter_32,
                                                          updater_t_counter_32_value);
                                                          
                                                          this.t_gensym_44 = t_gensym_80;
                                                           return (void(0));};
                        let t_gensym_45 = this.t_button_7.run(t_gensym_43,
                       t_gensym_44);
                       let updater_t_gensym_45 = () => {let t_gensym_79 = this.t_button_7.run(t_gensym_43,
                                                          t_gensym_44);
                                                          this.t_gensym_45 = t_gensym_79;
                                                           return (void(0));};
                        let t_gensym_46 = this.t_gensym_45.return;
                       let updater_t_gensym_46 = () => {let t_gensym_78 = this.t_gensym_45.return;
                                                          
                                                          this.t_gensym_46 = t_gensym_78;
                                                           return (void(0));};
                       
                       let updater_t_gensym_46_children_off_0 = this.t_gensym_45.updater_result_children_off_0;
                       
                       let updater_updater_t_gensym_46_children_off_0 = () => {
                       let t_gensym_77 = this.t_gensym_45.updater_result_children_off_0;
                         
                         this.updater_t_gensym_46_children_off_0 = t_gensym_77;
                          return (void(0));};
                       let t_gensym_47 = this.t_div_14;
                       let updater_t_gensym_47 = () => {let t_gensym_76 = this.t_div_14;
                                                          
                                                          this.t_gensym_47 = t_gensym_76;
                                                           return (void(0));};
                        let t_gensym_48 = this.t_counter_32.value;
                       let updater_t_gensym_48 = () => {let t_gensym_75 = this.t_counter_32.value;
                                                          
                                                          this.t_gensym_48 = t_gensym_75;
                                                           return (void(0));};
                        let t_gensym_49 = this.t_div_6.run(t_gensym_48);
                       let updater_t_gensym_49 = () => {let t_gensym_74 = this.t_div_6.run(t_gensym_48);
                                                          
                                                          this.t_gensym_49 = t_gensym_74;
                                                           return (void(0));};
                        let t_gensym_50 = this.t_gensym_49.return;
                       let updater_t_gensym_50 = () => {let t_gensym_73 = this.t_gensym_49.return;
                                                          
                                                          this.t_gensym_50 = t_gensym_73;
                                                           return (void(0));};
                       
                       let updater_t_gensym_50_children_off_0 = this.t_gensym_49.updater_result_children_off_0;
                       
                       let updater_updater_t_gensym_50_children_off_0 = () => {
                       let t_gensym_72 = this.t_gensym_49.updater_result_children_off_0;
                         
                         this.updater_t_gensym_50_children_off_0 = t_gensym_72;
                          return (void(0));};
                       let t_gensym_51 = this.t_div_5.run(t_gensym_46,
                       t_gensym_50);
                       let updater_t_gensym_51 = () => {let t_gensym_71 = this.t_div_5.run(t_gensym_46,
                                                          t_gensym_50);
                                                          this.t_gensym_51 = t_gensym_71;
                                                           return (void(0));};
                        let t_gensym_52 = this.t_gensym_51.return;
                       let updater_t_gensym_52 = () => {let t_gensym_70 = this.t_gensym_51.return;
                                                          
                                                          this.t_gensym_52 = t_gensym_70;
                                                           return (void(0));};
                       
                       let updater_t_gensym_52_children_off_0 = this.t_gensym_51.updater_result_children_off_0;
                       
                       let updater_updater_t_gensym_52_children_off_0 = () => {
                       let t_gensym_69 = this.t_gensym_51.updater_result_children_off_0;
                         
                         this.updater_t_gensym_52_children_off_0 = t_gensym_69;
                          return (void(0));};
                       let updater_t_gensym_52_children_off_1 = this.t_gensym_51.updater_result_children_off_1;
                       
                       let updater_updater_t_gensym_52_children_off_1 = () => {
                       let t_gensym_68 = this.t_gensym_51.updater_result_children_off_1;
                         
                         this.updater_t_gensym_52_children_off_1 = t_gensym_68;
                          return (void(0));}; return ({return: t_gensym_52});}}
class t_main_97 {constructor() {this.run = this.run.bind(this);}run() {
                   let updater_t_App_54 = () => {this.t_App_54 = t_gensym_96;
                                                   return (void(0));};
                     let t_gensym_55 = this.t_render_into_26;
                     let updater_t_gensym_55 = () => {let t_gensym_95 = this.t_render_into_26;
                                                        
                                                        this.t_gensym_55 = t_gensym_95;
                                                         return (void(0));};
                     let t_gensym_56 = "root";
                     let updater_t_gensym_56 = () => {let t_gensym_94 = "root";
                                                        
                                                        this.t_gensym_56 = t_gensym_94;
                                                         return (void(0));};
                     let t_gensym_57 = this.t_app_53;
                     let updater_t_gensym_57 = () => {let t_gensym_93 = this.t_app_53;
                                                        
                                                        this.t_gensym_57 = t_gensym_93;
                                                         return (void(0));};
                     let t_gensym_58 = this.t_App_54.run();
                     let updater_t_gensym_58 = () => {let t_gensym_92 = this.t_App_54.run();
                                                        
                                                        this.t_gensym_58 = t_gensym_92;
                                                         return (void(0));};
                     let t_gensym_59 = this.t_gensym_58.return;
                     let updater_t_gensym_59 = () => {let t_gensym_91 = this.t_gensym_58.return;
                                                        
                                                        this.t_gensym_59 = t_gensym_91;
                                                         return (void(0));};
                     let t_gensym_60 = this.t_render_into_8.run(t_gensym_56,
                     t_gensym_59);
                     let updater_t_gensym_60 = () => {let t_gensym_90 = this.t_render_into_8.run(t_gensym_56,
                                                        t_gensym_59);
                                                        this.t_gensym_60 = t_gensym_90;
                                                         return (void(0));};
                     let t_gensym_61 = this.t_gensym_60.return;
                     let updater_t_gensym_61 = () => {let t_gensym_89 = this.t_gensym_60.return;
                                                        
                                                        this.t_gensym_61 = t_gensym_89;
                                                         return (void(0));};
                     return (this.t_gensym_61);}}(new t_main_97()).run();