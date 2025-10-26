
// Idee pour la suite : mettre toutes les values et operateur dans une liste afin de calculer
// seulement au moment du egal pour respecter la priorite des operations
use eframe::{egui::{self, pos2, vec2, Button, Color32, Context, FontFamily, FontId, Label, Rect, Response, TextStyle}};
use reqwest::blocking::Client;
use serde_json::Value;


#[derive(Default)]
struct App{
    s: String,
}


impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, _ : &mut eframe::Frame) {
        set_styles(ctx);
        egui::CentralPanel::default().show(ctx, |ui|{
            ui.heading("Calculator");
            let res_pos = pos2(320.0, 170.0);
            let res_size = vec2(350.0, 20.0);
            let rect = Rect::from_min_size(res_pos, res_size);
            ui.put(rect, Label::new(&self.s).truncate());

            let cols = 4;
            let btn_size = vec2(100.0, 40.0);
            let spacing = vec2(5.0, 5.0); 
            let start_pos = pos2(260.0, 240.0);
     
            for i in 2..22 {
                let col = (i - 2) % cols;
                let row = (i - 2) / cols;
     
                let pos = pos2(
                    start_pos.x + (btn_size.x + spacing.x) * col as f32,
                    start_pos.y + (btn_size.y + spacing.y) * row as f32,
                );
     
                let r = Rect::from_min_size(pos, btn_size);
                match i {
                    2 => if ui.put(r, Button::new(" ").corner_radius(18.00)).clicked() {self.s = self.s.clone() + "0"},
                    3 => if  ui.put(r, Button::new("C").corner_radius(18.00)).clicked() {self.s.clear()},
                    4 => if ui.put(r, Button::new("%").corner_radius(18.00)).clicked() {self.s = self.s.clone() + "%"},
                    5 => if put_orange_button(ui, r, "/").clicked() {self.s = self.s.clone() + "/"},
                    9 => if put_orange_button(ui, r, "x").clicked() {self.s = self.s.clone() + "*"},
                    13 => if put_orange_button(ui, r, "-").clicked() {self.s = self.s.clone() + "-"},
                    17 => if put_orange_button(ui, r, "+").clicked() {self.s = self.s.clone() + "+"},
                    18 => if ui.put(r, Button::new("+/-").corner_radius(18.00)).clicked() {self.s = self.s.clone() + "i"},
                    19 => if ui.put(r, Button::new("0").corner_radius(18.00)).clicked() {self.s = self.s.clone() + "0"},
                    20 => if ui.put(r, Button::new(".").corner_radius(18.00)).clicked() {self.s = self.s.clone() + "."},
                    21 => if put_orange_button(ui, r, "=").clicked() {
                        self.s = compute_on_server(&self.s.clone()).unwrap();
                    },
                    _ => {
                        let number = col+1+(3-row)*3;
                        let c = ui.put(r, Button::new(number.to_string()).corner_radius(18.00));
                        if c.clicked() {self.s = self.s.clone() + &number.to_string()};
                    },
                };
            };
        });
    }
}

fn main() {
    let _ = eframe::run_native(
       "Calculator",
       eframe::NativeOptions { 
           viewport: egui::ViewportBuilder::default()
               .with_inner_size((400.0, 200.0)),
           ..Default::default()
       },
       Box::new(|_| Ok(Box::<App>::default())),
    );
}

fn set_styles(ctx: &Context){
    let mut style = (*ctx.style()).clone();
    style.text_styles = [
        (TextStyle::Heading, FontId::new(40.0, FontFamily::Monospace)),
        (TextStyle::Body, FontId::new(40.0, FontFamily::Monospace)),
        (TextStyle::Button, FontId::new(22.0, FontFamily::Monospace)),
        (TextStyle::Small, FontId::new(14.0, FontFamily::Monospace))
    ].into();
    ctx.set_style(style);
}

fn put_orange_button(ui: &mut egui::Ui, r: Rect, s: &str) -> Response{
   let b = Button::new(egui::RichText::new(s).color(Color32::from_rgb(80, 80, 80))).fill(Color32::from_rgb(255, 149, 0))
       .corner_radius(18.00);
    ui.put(r, b)
}

pub fn compute_on_server(expr: &str) -> Result<String, Box<dyn std::error::Error>> {
    let client = Client::new();
    let url = format!("http://localhost:8080/compute?exprStr={}", urlencoding::encode(expr));
    let resp = client.get(&url).send()?;
    let json: Value = resp.json()?;
    Ok(json["result"].as_str().unwrap_or("error").to_string())
}
