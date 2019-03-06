use glium::backend::Facade;
use glium::index::PrimitiveType;
use glium::{implement_vertex, IndexBuffer, VertexBuffer};

#[derive(Copy, Clone, Debug)]
pub struct Vertex {
    position: [f32; 2],
    color: [f32; 4],
}

implement_vertex!(Vertex, position, color);

pub struct DrawShapes {
    indices: Vec<u32>,
    vertices: Vec<Vertex>,
}

impl DrawShapes {
    pub fn new() -> DrawShapes {
        DrawShapes {
            indices: Vec::with_capacity(200),
            vertices: Vec::with_capacity(200),
        }
    }

    pub fn clear(&mut self) {
        self.indices.clear();
        self.vertices.clear();
    }

    pub fn fill_rect(&mut self, rect: &Rect, color: (f32, f32, f32, f32)) {
        let color = [color.0, color.1, color.2, color.3];
        let base_index = self.vertices.len() as u32;
        self.indices.push(base_index);
        self.indices.push(base_index + 1);
        self.indices.push(base_index + 2);
        self.indices.push(base_index + 1);
        self.indices.push(base_index + 3);
        self.indices.push(base_index + 2);
        self.vertices.push(Vertex {
            position: [rect.left, rect.top],
            color,
        });
        self.vertices.push(Vertex {
            position: [rect.right, rect.top],
            color,
        });
        self.vertices.push(Vertex {
            position: [rect.left, rect.bottom],
            color,
        });
        self.vertices.push(Vertex {
            position: [rect.right, rect.bottom],
            color,
        });
    }

    pub fn finish<F: Facade>(&mut self, facade: &F) -> FinishedShapes {
        let vertices = VertexBuffer::new(facade, &self.vertices).unwrap();
        let indices =
            IndexBuffer::new(facade, PrimitiveType::TrianglesList, &self.indices).unwrap();
        FinishedShapes {
            vertices,
            indices,
        }
    }
}

pub struct FinishedShapes {
    pub vertices: VertexBuffer<Vertex>,
    pub indices: IndexBuffer<u32>,
}

pub struct Rect {
    pub left: f32,
    pub top: f32,
    pub right: f32,
    pub bottom: f32,
}
