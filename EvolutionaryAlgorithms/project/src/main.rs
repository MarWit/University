use std::io::prelude::*;
use std::time::Instant;

use image::RgbaImage;
use imageproc::drawing::{self, Canvas};
use ndarray::parallel::prelude::*;
use ndarray::prelude::*;
use ndarray_rand::RandomExt;
use rand::prelude::*;
use rand_distr::{StandardNormal, Uniform, WeightedIndex};

type Scalar = f32;
type Point = imageproc::point::Point<i32>;

#[derive(Clone, Copy)]
struct Transformation {
    pub angle: Scalar,
    pub displacement: (i32, i32),
    sincos: (f32, f32),
}

#[derive(Clone)]
enum Object {
    #[allow(dead_code)]
    Triangle {
        w: i32,
        h: i32,
    },
    #[allow(dead_code)]
    Square {
        w: i32,
    },
    Polygon {
        center: Point,
        points: Vec<Point>,
    },
    Circle {
        center: Point,
        radius: i32,
    },
}

impl Transformation {
    fn new(angle: f32, displacement: (i32, i32)) -> Self {
        Self {
            angle,
            sincos: angle.sin_cos(),
            displacement,
        }
    }

    #[allow(dead_code)]
    fn set_angle(&mut self, angle: Scalar) {
        self.angle = angle;
        self.sincos = self.angle.sin_cos();
    }

    fn transform(&self, point: Point) -> Point {
        let (px, py) = (point.x as Scalar, point.y as Scalar);
        let (s, c) = self.sincos;

        Point::new(
            (px * c - py * s) as i32 + self.displacement.0,
            (px * s + py * c) as i32 + self.displacement.1,
        )
    }
}

impl Object {
    pub fn draw<C>(&self, trans: Transformation, color: C::Pixel, canvas: &mut C)
    where
        C: Canvas,
        C::Pixel: 'static,
    {
        use Object::*;

        match self {
            Triangle { w, h } => {
                let points = vec![Point::new(0, 0), Point::new(0, -h), Point::new(*w, 0)]
                    .into_iter()
                    .map(|p| trans.transform(p))
                    .collect::<Vec<_>>();

                drawing::draw_polygon_mut(canvas, &points, color);
            }
            Square { w } => {
                let points = vec![
                    Point::new(0, 0),
                    Point::new(0, -w),
                    Point::new(*w, -w),
                    Point::new(*w, 0),
                ]
                .into_iter()
                .map(|p| trans.transform(p))
                .collect::<Vec<_>>();

                drawing::draw_polygon_mut(canvas, &points, color);
            }
            Polygon { points, center: _ } => {
                let points = points
                    .iter()
                    .map(|&p| trans.transform(p))
                    .collect::<Vec<_>>();

                drawing::draw_polygon_mut(canvas, &points, color);
            }
            Circle { center, radius } => {
                let center = trans.transform(*center);
                drawing::draw_filled_circle_mut(canvas, (center.x, center.y), *radius, color);
            }
        }
    }

    fn is_ok(&self, trans: Transformation) -> bool {
        use Object::*;

        match self {
            Triangle { w, h: _ } => {
                let points = vec![Point::new(0, 0), Point::new(*w, 0)]
                    .into_iter()
                    .map(|p| trans.transform(p))
                    .collect::<Vec<_>>();

                points[0] != points[1]
            }
            Polygon { points, center: _ } => {
                trans.transform(points[0]) != trans.transform(*points.last().unwrap())
            }
            _ => true,
        }
    }
}

fn fitness(a: &RgbaImage, b: &RgbaImage) -> f32 {
    a.pixels()
        .zip(b.pixels())
        .map(|(p1, p2)| {
            p1.0.iter()
                .zip(p2.0.iter())
                .map(|(&v, &w)| (v as f32 - w as f32).powi(2))
                .sum::<f32>()
        })
        .sum()
}

fn standard_matrix(w: usize, h: usize, mut rng: impl Rng) -> Array2<Scalar> {
    Array2::random_using((w, h), StandardNormal, &mut rng)
}

fn es<F>(
    f: F,
    domain: &[(Scalar, Scalar)],
    problem_size: usize,
    mu: usize,
    lambda: usize,
    k: Scalar,
    iterations_num: usize,
    mut rng: impl Rng,
) -> (
    Array1<Scalar>,
    Scalar,
    Vec<Array1<Scalar>>,
    Vec<Array1<Scalar>>,
)
where
    F: Fn(ArrayView1<Scalar>) -> Scalar + Send + Sync,
{
    let mut p = Array2::zeros((mu, problem_size));
    for i in 0..problem_size {
        let col = Array1::random_using(
            mu,
            Uniform::new_inclusive(domain[i].0, domain[i].1),
            &mut rng,
        );
        p.slice_mut(s![.., i]).assign(&col);
    }

    let mut p_scores = Array1::from(
        p.view()
            .outer_iter()
            .into_par_iter()
            .map(&f)
            .collect::<Vec<_>>(),
    );
    let mut sigma = Array2::from_elem((mu, problem_size), 1.0);

    let tau = k / ((2 * problem_size) as Scalar).sqrt();
    let tau0 = k / (2.0 * (problem_size as Scalar).sqrt()).sqrt();

    // let mut rng = rand::thread_rng();
    let now = Instant::now();
    let mut history = vec![];
    let mut sigma_history = vec![];

    for iter in 1..=iterations_num {
        print!(
            "\r{}/{}, {}ms/iter",
            iter,
            iterations_num,
            (now.elapsed().as_micros() / (iter as u128)) as f32 / 1000.0
        );

        let best_idx = p_scores
            .iter()
            .enumerate()
            .min_by(|a, b| a.1.partial_cmp(&b.1).unwrap())
            .unwrap()
            .0;
        history.push(p.slice(s![best_idx, ..]).into_owned());
        sigma_history.push(sigma.slice(s![best_idx, ..]).into_owned());

        // Parent selection
        let mut probability = -p_scores.clone()
            + *p_scores
                .iter()
                .max_by(|a, b| a.partial_cmp(&b).unwrap())
                .unwrap();

        if probability.sum() > 0.0 {
            probability /= probability.sum();
        }
        if probability.sum() <= 0.0 {
            probability = Array1::ones(mu) / mu as Scalar;
        }

        let dist = WeightedIndex::new(&probability).expect("weighted_index");
        let mut children = Array2::zeros((lambda, problem_size));
        let mut children_sigma = Array2::zeros((lambda, problem_size));
        let mut count = 0;
        for idx in dist.sample_iter(&mut rng) {
            children
                .slice_mut(s![count, ..])
                .assign(&p.row(idx).to_owned());
            children_sigma
                .slice_mut(s![count, ..])
                .assign(&sigma.row(idx).to_owned());
            count += 1;

            if count == lambda {
                break;
            }
        }

        // Mutation
        let epsilon_i = standard_matrix(lambda, problem_size, &mut rng) * tau;
        let epsilon_0 = standard_matrix(lambda, 1, &mut rng) * tau0;
        let mut epsilon = epsilon_i + epsilon_0;
        epsilon.iter_mut().for_each(|v| *v = v.exp());

        children_sigma = children_sigma * epsilon;
        children =
            children + standard_matrix(lambda, problem_size, &mut rng) * children_sigma.clone();

        let children_scores = Array1::from(
            children
                .view()
                .outer_iter()
                .into_par_iter()
                .map(&f)
                .collect::<Vec<_>>(),
        );

        let mut scores = children_scores
            .iter()
            .enumerate()
            .map(|(i, v)| (*v, i, true))
            .chain(p_scores.iter().enumerate().map(|(i, v)| (*v, i, false)))
            .collect::<Vec<_>>();

        scores.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());
        let mut i = 0;
        for (_, idx, is_child) in scores.into_iter() {
            let (v, s) = if is_child {
                (
                    children.row(idx).to_owned(),
                    children_sigma.row(idx).to_owned(),
                )
            } else {
                (p.row(idx).to_owned(), sigma.row(idx).to_owned())
            };

            if v.iter()
                .enumerate()
                .any(|(i, &e)| e < domain[i].0 || e > domain[i].1)
            {
                continue;
            }

            p.slice_mut(s![i, ..]).assign(&v);
            sigma.slice_mut(s![i, ..]).assign(&s);

            i += 1;
            if i == mu {
                break;
            }
        }

        p_scores = Array1::from(
            p.view()
                .outer_iter()
                .into_par_iter()
                .map(&f)
                .collect::<Vec<_>>(),
        );
    }

    println!();

    p_scores
        .into_iter()
        .enumerate()
        .min_by(|a, b| a.1.partial_cmp(b.1).unwrap())
        .map(|(i, s)| (p.row(i).to_owned(), *s, history, sigma_history))
        .expect("p_scores")
}

fn main() {
    let mut rng = rand::rngs::StdRng::seed_from_u64(1234);

    let original = image::io::Reader::open("originals/girl_with_pearl.jpg")
        .unwrap()
        .decode()
        .unwrap()
        .into_rgba8();

    let w = original.width() as i32;
    let h = original.height() as i32;

    let worst_score = (255.0 * 255.0) * 3. * w as f32 * h as f32;
    let mut image = image::RgbaImage::new(w as _, h as _);
    image
        .pixels_mut()
        .for_each(|p| *p = image::Rgba([255, 255, 255, 255]));

    let mut last_fit = fitness(&original, &image);
    let f = |img: RgbaImage, object: Object| {
        move |v: ArrayView1<Scalar>| {
            if let Some([scale, angle, x, y, r, g, b]) = v.as_slice() {
                let mut figure = object.clone();
                let min_r = w.min(h) as f32;
                match &mut figure {
                    Object::Polygon { center, points } => {
                        for point in points.iter_mut() {
                            point.x = ((point.x - center.x) as f32 * scale) as i32 + center.x;
                            point.y = ((point.y - center.y) as f32 * scale) as i32 + center.y;
                        }
                    }
                    Object::Circle { center: _, radius } => {
                        let scale = scale / 12.0;
                        *radius = (min_r * scale).max(5.0) as i32;
                    }
                    _ => unimplemented!(),
                };

                let transform = Transformation::new(*angle, (*x as i32, *y as i32));

                if !figure.is_ok(transform) {
                    return 1.0;
                }

                let color = image::Rgba([*r as u8, *g as u8, *b as u8, 255]);
                let mut c = img.clone();

                figure.draw(transform, color, &mut c);
                fitness(&original, &c) / worst_score
            } else {
                panic!("wtf")
            }
        }
    };

    let mut j = 1;
    for _ in 1.. {
        let mut figure = if true {
            // let mut rng = rand::thread_rng();
            let n_points = rng.gen_range(3..=20);
            let mut points = vec![];
            let (mut cx, mut cy) = (0, 0);
            for _ in 0..n_points {
                let x = rng.gen_range(10..=w / 2);
                let y = rng.gen_range(10..=h / 2);
                cx += x;
                cy += y;
                points.push(Point::new(x, y));
            }

            cx /= n_points as i32;
            cy /= n_points as i32;

            for point in &mut points {
                point.x -= cx;
                point.y -= cy;
            }

            points.sort_by(|p1, p2| {
                let angle1 = (p1.y as f32).atan2(p1.x as f32);
                let angle2 = (p2.y as f32).atan2(p2.x as f32);
                angle1.partial_cmp(&angle2).unwrap()
            });
            Object::Polygon {
                center: Point::new(0, 0),
                points,
            }
        } else {
            Object::Circle {
                radius: 0,
                center: Point::new(0, 0),
            }
        };

        let func = f.clone()(image.clone(), figure.clone());
        let (res, fit, history, sigma_history) = es(
            &func,
            &[
                (0.05, 3.0),
                (0., 2. * 3.1415),
                (0., w as f32),
                (0., h as f32),
                (0., 255.),
                (0., 255.),
                (0., 255.),
            ],
            7,
            10,
            10,
            5.0,
            200,
            &mut rng,
        );

        if fit < last_fit {
            if (j % 50) == 1 {
                // batch_fit = fit;
                let _ = std::fs::create_dir_all(&format!("output/img{:04}", j));
                let copy_image = image.clone();
                let copy_figure = figure.clone();
                std::thread::spawn(move || {
                    let mut f =
                        std::fs::File::create(&format!("output/img{:04}/individual", j)).unwrap();
                    for (n, r) in history.into_iter().enumerate() {
                        let mut new_image = copy_image.clone();
                        let mut new_figure = copy_figure.clone();
                        if let Some([scale, angle, x, y, r, g, b]) = r.as_slice() {
                            let min_r = w.min(h) as f32;
                            match &mut new_figure {
                                Object::Polygon { center, points } => {
                                    for point in points.iter_mut() {
                                        point.x =
                                            ((point.x - center.x) as f32 * scale) as i32 + center.x;
                                        point.y =
                                            ((point.y - center.y) as f32 * scale) as i32 + center.y;
                                    }
                                }
                                Object::Circle { center: _, radius } => {
                                    let scale = scale / 12.0;
                                    *radius = (min_r * scale).max(5.0) as i32;
                                }
                                _ => unimplemented!(),
                            };

                            let transform = Transformation::new(*angle, (*x as i32, *y as i32));
                            if !new_figure.is_ok(transform) {
                                continue;
                            }

                            let color = image::Rgba([*r as u8, *g as u8, *b as u8, 255]);

                            new_figure.draw(transform, color, &mut new_image);
                            let _ = new_image.save(&format!("output/img{:04}/{:04}.jpg", j, n));
                        }

                        let line = r
                            .into_iter()
                            .map(ToString::to_string)
                            .collect::<Vec<String>>()
                            .join(" ");
                        let _ = writeln!(&mut f, "{}", line);
                    }

                    f = std::fs::File::create(&format!("output/img{:04}/sigmas", j)).unwrap();
                    for s in sigma_history {
                        let line = s
                            .into_iter()
                            .map(ToString::to_string)
                            .collect::<Vec<String>>()
                            .join(" ");

                        let _ = writeln!(&mut f, "{}", line);
                    }
                });
            }

            if let Some([scale, angle, x, y, r, g, b]) = res.as_slice() {
                let min_r = w.min(h) as f32;
                match &mut figure {
                    Object::Polygon { center, points } => {
                        for point in points.iter_mut() {
                            point.x = ((point.x - center.x) as f32 * scale) as i32 + center.x;
                            point.y = ((point.y - center.y) as f32 * scale) as i32 + center.y;
                        }
                    }
                    Object::Circle { center: _, radius } => {
                        let scale = scale / 12.0;
                        *radius = (min_r * scale).max(5.0) as i32;
                    }
                    _ => unimplemented!(),
                };

                let transform = Transformation::new(*angle, (*x as i32, *y as i32));
                let color = image::Rgba([*r as u8, *g as u8, *b as u8, 255]);

                figure.draw(transform, color, &mut image);
            }

            last_fit = fit;
            let _ = image.save(&format!("output/img{:04}.jpg", j));
            j += 1;
        }

        println!(
            "{} -> {:.2}% ({:.2}%)",
            res,
            (1. - fit) * 100.,
            (1. - last_fit) * 100.
        );
    }
}