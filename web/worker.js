var w;
var y;
var data;

self.addEventListener('message', function(e) {
    w = e.data[0];
    const h = e.data[1];
    y = e.data[2];
    const linesPerWorker = e.data[3];
    data = new Uint8ClampedArray(w*linesPerWorker*4);
	
	initObjects();
    
	for (let yw = y; yw < y+linesPerWorker; yw++)
	{
		for (let x = 0; x < w; x++) 
		{
			let u = -(x - w / 2) / w * 1.6 * (w / h);
			let v = -(yw - h / 2) / h * 1.6;
			let ro = new Vec3(0, 15, -60);                  // rotated position
			let rd = (new Vec3(-u, v, 1)).normalize();
			Scheduler.addTask(new RenderTaskViewport(x, yw, ro, rd));			
		}
	}
	runScheduler();
	
	self.postMessage(data);
});


function sdf(p) 
{
	let minD = FAR;
	let minDobj = null;
	for(let i = 0; i < objects.length; i++)
	{
		let d = objects[i].sdf(p);
		if(d < minD)
		{
			minD = d;
			minDobj = objects[i];
		}
	}
	if(minDobj)
		return new Collision(minD, p, vec(0, 0, 0), minDobj);
	return null;
}

function getNormal(coll) {
    const E = 0.01;
	const dx = coll.obj.sdf(coll.p.add(vec(E, 0, 0))) - coll.obj.sdf(coll.p.sub(vec(E, 0, 0)));
	const dy = coll.obj.sdf(coll.p.add(vec(0, E, 0))) - coll.obj.sdf(coll.p.sub(vec(0, E, 0)));
	const dz = coll.obj.sdf(coll.p.add(vec(0, 0, E))) - coll.obj.sdf(coll.p.sub(vec(0, 0, E)));
    return vec(dx, dy, dz).normalize();
}

function getLight(eyeDir, normal, position, lightPos, baseColor, mat) {
    let lightDir = lightPos.sub(position).normalize();
    let diff = Math.max(normal.dot(lightDir), 0);
    let halfVec = lightDir.sub(eyeDir).normalize();
    let spec = Math.pow(Math.max(halfVec.dot(normal), 0), 16);
    let lightColor = vec(1, 1, 1);
    return baseColor.scale(mat.ambient).
		add(baseColor.scale(diff * mat.diffuse)).
		add(lightColor.scale(spec * mat.specular));
}

function isInShadow(p, lightPos) {
    let dir = lightPos.sub(p).normalize();
    let distToLight = lightPos.sub(p).length();
    let shadowP = p.add(dir.scale(0.2));
    for (let i = 0; i < 50; i++) {
        let c = sdf(shadowP);
		if(c)
		{
			if (c.d < 0.01) return true;
			shadowP = shadowP.add(dir.scale(c.d));
			if (shadowP.sub(p).length > distToLight) break;
		}
	}
    return false;
}

function getReflection(origin, dir, depth) {
    let t = 0;
    for (let i = 0; i < 64; i++) {
        let p = origin.add(dir.scale(t));
        let coll = sdf(p);
		if(!coll) return vec(0, 0, 0);
        if (coll.d < 0.02) {
            coll.n = getNormal(coll);
            let color = coll.obj.mat.shade(coll.p, coll.n);
            let lightPos = new Vec3(0, 40, -20);
            let inShadow = isInShadow(p, lightPos);
            //let shaded = color;
			let shaded = getLight(dir, coll.n, coll.p, lightPos, color, coll.obj.mat);
            if (inShadow) shaded = color.scale(coll.obj.mat.ambient);
            if (depth > 0 && coll.obj.mat.reflection > 0.0) 
			{
                let refl = getReflection(p.add(coll.n.scale(0.1)), dir.reflect(coll.n), depth - 1);
                shaded = shaded.mix(coll.obj.mat.reflection, refl);
            }
            return shaded;
        }
        t += coll.d;
        if (t > 200) break;
    }
    return vec(0, 0, 0);
}


function runScheduler()
{
while(Scheduler.tasks.length) {
	for(let i = 0; i < /*1024*/Scheduler.tasks.length; i++)
	{
		if(!Scheduler.execute()) break;
	}
/*	ctx.putImageData(imageData, 0, 0);	
	if(Scheduler.tasks.length)
		window.setTimeout(runScheduler, 1);*/
}
}

class Scheduler
{
	static tasks = [];
	static addTask(task, prio = 0)
	{
		if(prio)
			this.tasks.push(task);
		else
			this.tasks.unshift(task);
	}

	static execute()
	{
		if(!this.tasks.length) return false;
		this.tasks.pop().execute();
		return true;
	}
}

class RenderTask
{
	constructor(parent)
	{
		this.parent = parent;
	}

	/*execute()
	{
	}*/
}

class RenderTaskViewport extends RenderTask
{
	constructor(x, y, pos, dir)
	{
		super(null);
		this.x = x;
		this.y = y;
		this.pos = pos;
		this.dir = dir;
	}

	execute()
	{
		const MAX_DEPTH = 4;
		Scheduler.addTask(new RenderTaskColor(this, this.pos, this.dir, MAX_DEPTH), 1);
	}

	returnColor(color)
	{
//		const w = canvas.width;
		color = color.scale(255).clamp(0, 255);
		let i = ((this.y-y) * w + this.x) * 4;
		data[i + 0] = color.v[0];
		data[i + 1] = color.v[1];
		data[i + 2] = color.v[2];
		data[i + 3] = 255;
	}
}

class RenderTaskColor extends RenderTask
{
	constructor(parent, pos, dir, depth)
	{
		super(parent);
		this.pos = pos;
		this.dir = dir;
		this.depth = depth;
		this.waitingForCollision = true;
		this.waitingForShadow = false;
		this.waitingForReflection = false;
		this.coll = null;
		this.distToLight = 0;
		this.shaded = true;
	}

	execute()
	{
		const MAX_DISTANCE = 200;
		const MAX_STEPS = 64;
		Scheduler.addTask(new RenderTaskCollision(this, this.pos, this.dir, MAX_DISTANCE, MAX_STEPS), 1);
	}

	returnCollision(coll, t)
	{
		/*if(coll)
		{
			this.parent.returnColor(coll.obj.mat.color);
			return;
		}*/

		let lightPos = new Vec3(0, 40, -20);
		if(this.waitingForCollision)
		{
			this.waitingForCollision = false;
			const MAX_SHADOW_STEPS = 64;
			if(!coll)
			{
				this.parent.returnColor(new Vec3(0, 0, 0));
				return;
			}
			coll.n = getNormal(coll);
			this.coll = coll;

			let lightDir = lightPos.sub(coll.p).normalize();
			this.distToLight = lightPos.sub(coll.p).length();

			this.waitingForShadow = true;
			Scheduler.addTask(new RenderTaskShadow(this, coll.p.add(lightDir.scale(0.2)), lightDir, this.distToLight + 0.1, MAX_SHADOW_STEPS), 1);
		}
		else if(this.waitingForShadow)
		{
			this.waitingForShadow = false;
			let color = this.coll.obj.mat.shade(this.coll.p, this.coll.n);
			let inShadow = false;
			if(coll !== null && (coll.d < 0.01))
				inShadow = true;
			if (inShadow) 
				this.shaded = color.scale(this.coll.obj.mat.ambient);
			else
				this.shaded = getLight(this.dir, this.coll.n, this.coll.p, lightPos, color, this.coll.obj.mat);

			if (this.depth > 0 && this.coll.obj.mat.reflection > 0.0)
			{
				this.waitingForReflection = true;
				Scheduler.addTask(new RenderTaskColor(this, this.coll.p.add(this.coll.n.scale(0.1)), this.dir.reflect(this.coll.n), this.depth - 1), 1);
			}
			else
				this.parent.returnColor(this.shaded);
		}
		
	}

	returnColor(color)
	{
		if(this.waitingForReflection)
		{
			this.waitingForReflection = false;
			this.shaded = this.shaded.mix(this.coll.obj.mat.reflection, color);
			this.parent.returnColor(this.shaded);
		}
	}
}

class RenderTaskCollision extends RenderTask
{
	constructor(parent, pos, dir, maxDistance, maxSteps)
	{
		super(parent);
		this.pos = pos;
		this.dir = dir;
		this.maxDistance = maxDistance;
		this.maxSteps = maxSteps;
	}

	execute()
	{
		let t = 0;
    	for (let i = 0; i < this.maxSteps; i++) 
		{
        	let p = this.pos.add(this.dir.scale(t));
        	let coll = sdf(p);
			if(!coll) break;
			if (coll.d < 0.02) 
			{
				this.parent.returnCollision(coll, t);
				return;
			}
			//t += this.dir.scale(coll.d);
			t += coll.d;
			if (t > this.maxDistance) break;
		}
		this.parent.returnCollision(null, 0);
	}
}

class RenderTaskShadow extends RenderTask
{
	constructor(parent, pos, dir, maxDistance, maxSteps)
	{
		super(parent);
		this.pos = pos;
		this.dir = dir;
		this.maxDistance = maxDistance;
		this.maxSteps = maxSteps;
	}

	execute()
	{
		let coll = null;
		let t = 0;
		let shadowP = this.pos;
    	for (let i = 0; i < this.maxSteps; i++) 
		{
        	coll = sdf(shadowP);
			if(!coll) break;
			if (coll.d < 0.01) break;
			shadowP = shadowP.add(this.dir.scale(coll.d));
			if (shadowP.sub(this.pos).length > this.maxDistance) break;
		}
		this.parent.returnCollision(coll, t);
	}
}

let objects = [];

class Vec3
{
	constructor(x, y, z){
		this.v = [x, y, z];
	}
	length(){
		return Math.sqrt(this.v[0]**2 + this.v[1]**2 + this.v[2]**2);
	}
	normalize(){
		let l = this.length(); 
		if(l === 0) return;
		return new Vec3(...this.v.map(x => x / l));
	}
	sub(v2){
		return new Vec3(this.v[0] - v2.v[0], this.v[1] - v2.v[1], this.v[2] - v2.v[2]);
	}
	add(v2){
		return new Vec3(this.v[0] + v2.v[0], this.v[1] + v2.v[1], this.v[2] + v2.v[2]);
	}
	scale(s){
		return new Vec3(...this.v.map(x => x * s));
	}
	dot(v2){
		return this.v[0] * v2.v[0] + this.v[1] * v2.v[1] + this.v[2] * v2.v[2];
	}
	reflect(n){
		return this.sub(n.scale(2 * this.dot(n)));
	}
	clamp(a, b){
		return new Vec3(Math.max(a, Math.min(b, this.v[0])), Math.max(a, Math.min(b, this.v[1])), Math.max(a, Math.min(b, this.v[2])));
	}
	abs(){
		return new Vec3(Math.abs(this.v[0]), Math.abs(this.v[1]), Math.abs(this.v[2]));
	}
	mix(s, v){
		return v.scale(s).add(this.scale(1 - s));
	}
};

function vec(x, y, z)
{
	return new Vec3(x, y, z);
}

/*function sphere(pos, radius, mat)
{
}*/

const FAR = 32767;
class RenderObject
{
	constructor(pos, mat)
	{
		this.pos = pos;
		this.mat = mat;
	}

	sdf(p)
	{
		return FAR;
	}
}

class Renderer
{
}

class Collision
{
	constructor(d, p, n, obj)
	{
		this.d = d;
		this.p = p;
		this.n = n;
		this.obj = obj;
	}
}

class Sphere extends RenderObject
{
	constructor(pos, r, mat)
	{
		super(pos, mat);
		this.r = r;
	}

	sdf(p)
	{
		return p.sub(this.pos).length() - this.r;
	}
}

class Box extends RenderObject
{
	constructor(pos, dim, mat)
	{
		super(pos, mat);
		this.dim = dim;
	}

	sdf(p)
	{
		let d = p.sub(this.pos).abs().sub(this.dim);
    	let dist = Math.max(...d.v);
    	return dist;
	}
}

class Cylinder extends RenderObject
{
	constructor(pos, dim, mat)
	{
		super(pos, mat);
		this.dim = dim;
	}

	sdf(p)
	{
		let rel = p.sub(this.pos);
		let dxz = Math.sqrt(rel.v[0]**2 + rel.v[2]**2) - this.dim.v[0];
		let dy = Math.max(rel.v[1] - this.dim.v[1], -rel.v[1] - this.dim.v[1]);
		return Math.max(dxz, dy);
	}
}

class PlaneX extends RenderObject
{
	constructor(pos, mat)
	{
		super(pos, mat);
	}

	sdf(p)
	{
		return p.v[0] - this.pos.v[0];
	}
}

class PlaneY extends RenderObject
{
	constructor(pos, mat)
	{
		super(pos, mat);
	}

	sdf(p)
	{
		return p.v[1] - this.pos.v[1];
	}
}

class PlaneZ extends RenderObject
{
	constructor(pos, mat)
	{
		super(pos, mat);
	}

	sdf(p)
	{
		return p.v[2] - this.pos.v[2];
	}
}

class Material
{
	constructor(color, ambient, diffuse, specular, reflection)
	{
		this.color = color;
		this.ambient = ambient;
		this.diffuse = diffuse;
		this.specular = specular;
		this.reflection = reflection;
	}

	shade(p, n)
	{
		return this.color;
	}
}

class Checker extends Material
{
	constructor(color1, color2, ambient, diffuse, specular, reflection)
	{
		super(color1, ambient, diffuse, specular, reflection);
		this.color2 = color2;
	}

	shade(p, n)
	{
		let check = (Math.floor(p.v[0] / 10) + Math.floor(p.v[2] / 10)) % 2;
	    let color = check ? this.color : this.color2; // white/red
		return color;
	}
}

function initObjects()
{
	//objects.push(new Sphere(vec(0, 40, -20), 10, new Material(vec(1, 1, 1), 1, 0.1, 0.1, 0.0)));
	objects.push(new Sphere(vec(5, 7, 10), 15, new Material(vec(1, 0.2, 0.3), 0.2, 0.8, 0.8, 0.5)));
	objects.push(new Box(vec(25, 0, -15), vec(10, 10, 10), new Material(vec(0.4, 0.4, 1.0), 0.2, 0.8, 0.2, 0.2)));
	objects.push(new Cylinder(vec(-30, 10, -10), vec(10, 20, 0), new Material(vec(0.3, 0.7, 0.3), 0.2, 0.8, 0.2, 0.2)));
	objects.push(new PlaneY(vec(0, -10, 0), new Checker(vec(0.6, 0.1, 0.1), vec(1, 1, 1), 0.2, 0.8, 0.1, 0.2)));
}
