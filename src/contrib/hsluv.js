// Copyright (c) 2012-2022 Alexei Boronine

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// Compiled from https://github.com/hsluv/hsluv-javascript v1.0.0. Feature set
// is frozen. Also available as an NPM dep, added to source controle to avoid an
// extra NPM step for demos. TODO consider porting to cljc.
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.Hsluv = void 0;
class Hsluv {
    constructor() {
        // RGB
        this.hex = '#000000';
        this.rgb_r = 0;
        this.rgb_g = 0;
        this.rgb_b = 0;
        // CIE XYZ
        this.xyz_x = 0;
        this.xyz_y = 0;
        this.xyz_z = 0;
        // CIE LUV
        this.luv_l = 0;
        this.luv_u = 0;
        this.luv_v = 0;
        // CIE LUV LCh
        this.lch_l = 0;
        this.lch_c = 0;
        this.lch_h = 0;
        // HSLuv
        this.hsluv_h = 0;
        this.hsluv_s = 0;
        this.hsluv_l = 0;
        // HPLuv
        this.hpluv_h = 0;
        this.hpluv_p = 0;
        this.hpluv_l = 0;
        // 6 lines in slope-intercept format: R < 0, R > 1, G < 0, G > 1, B < 0, B > 1
        this.r0s = 0;
        this.r0i = 0;
        this.r1s = 0;
        this.r1i = 0;
        this.g0s = 0;
        this.g0i = 0;
        this.g1s = 0;
        this.g1i = 0;
        this.b0s = 0;
        this.b0i = 0;
        this.b1s = 0;
        this.b1i = 0;
    }
    static fromLinear(c) {
        if (c <= 0.0031308) {
            return 12.92 * c;
        }
        else {
            return 1.055 * Math.pow(c, 1 / 2.4) - 0.055;
        }
    }
    static toLinear(c) {
        if (c > 0.04045) {
            return Math.pow((c + 0.055) / 1.055, 2.4);
        }
        else {
            return c / 12.92;
        }
    }
    static yToL(Y) {
        if (Y <= Hsluv.epsilon) {
            return Y / Hsluv.refY * Hsluv.kappa;
        }
        else {
            return 116 * Math.pow(Y / Hsluv.refY, 1 / 3) - 16;
        }
    }
    static lToY(L) {
        if (L <= 8) {
            return Hsluv.refY * L / Hsluv.kappa;
        }
        else {
            return Hsluv.refY * Math.pow((L + 16) / 116, 3);
        }
    }
    static rgbChannelToHex(chan) {
        const c = Math.round(chan * 255);
        const digit2 = c % 16;
        const digit1 = (c - digit2) / 16 | 0;
        return Hsluv.hexChars.charAt(digit1) + Hsluv.hexChars.charAt(digit2);
    }
    static hexToRgbChannel(hex, offset) {
        const digit1 = Hsluv.hexChars.indexOf(hex.charAt(offset));
        const digit2 = Hsluv.hexChars.indexOf(hex.charAt(offset + 1));
        const n = digit1 * 16 + digit2;
        return n / 255.0;
    }
    static distanceFromOriginAngle(slope, intercept, angle) {
        const d = intercept / (Math.sin(angle) - slope * Math.cos(angle));
        if (d < 0) {
            return Infinity;
        }
        else {
            return d;
        }
    }
    static distanceFromOrigin(slope, intercept) {
        return Math.abs(intercept) / Math.sqrt(Math.pow(slope, 2) + 1);
    }
    static min6(f1, f2, f3, f4, f5, f6) {
        return Math.min(f1, Math.min(f2, Math.min(f3, Math.min(f4, Math.min(f5, f6)))));
    }
    rgbToHex() {
        this.hex = "#";
        this.hex += Hsluv.rgbChannelToHex(this.rgb_r);
        this.hex += Hsluv.rgbChannelToHex(this.rgb_g);
        this.hex += Hsluv.rgbChannelToHex(this.rgb_b);
    }
    hexToRgb() {
        this.hex = this.hex.toLowerCase();
        this.rgb_r = Hsluv.hexToRgbChannel(this.hex, 1);
        this.rgb_g = Hsluv.hexToRgbChannel(this.hex, 3);
        this.rgb_b = Hsluv.hexToRgbChannel(this.hex, 5);
    }
    xyzToRgb() {
        this.rgb_r = Hsluv.fromLinear(Hsluv.m_r0 * this.xyz_x + Hsluv.m_r1 * this.xyz_y + Hsluv.m_r2 * this.xyz_z);
        this.rgb_g = Hsluv.fromLinear(Hsluv.m_g0 * this.xyz_x + Hsluv.m_g1 * this.xyz_y + Hsluv.m_g2 * this.xyz_z);
        this.rgb_b = Hsluv.fromLinear(Hsluv.m_b0 * this.xyz_x + Hsluv.m_b1 * this.xyz_y + Hsluv.m_b2 * this.xyz_z);
    }
    rgbToXyz() {
        const lr = Hsluv.toLinear(this.rgb_r);
        const lg = Hsluv.toLinear(this.rgb_g);
        const lb = Hsluv.toLinear(this.rgb_b);
        this.xyz_x = 0.41239079926595 * lr + 0.35758433938387 * lg + 0.18048078840183 * lb;
        this.xyz_y = 0.21263900587151 * lr + 0.71516867876775 * lg + 0.072192315360733 * lb;
        this.xyz_z = 0.019330818715591 * lr + 0.11919477979462 * lg + 0.95053215224966 * lb;
    }
    xyzToLuv() {
        const divider = this.xyz_x + 15 * this.xyz_y + 3 * this.xyz_z;
        let varU = 4 * this.xyz_x;
        let varV = 9 * this.xyz_y;
        if (divider !== 0) {
            varU /= divider;
            varV /= divider;
        }
        else {
            varU = NaN;
            varV = NaN;
        }
        this.luv_l = Hsluv.yToL(this.xyz_y);
        if (this.luv_l === 0) {
            this.luv_u = 0;
            this.luv_v = 0;
        }
        else {
            this.luv_u = 13 * this.luv_l * (varU - Hsluv.refU);
            this.luv_v = 13 * this.luv_l * (varV - Hsluv.refV);
        }
    }
    luvToXyz() {
        if (this.luv_l === 0) {
            this.xyz_x = 0;
            this.xyz_y = 0;
            this.xyz_z = 0;
            return;
        }
        const varU = this.luv_u / (13 * this.luv_l) + Hsluv.refU;
        const varV = this.luv_v / (13 * this.luv_l) + Hsluv.refV;
        this.xyz_y = Hsluv.lToY(this.luv_l);
        this.xyz_x = 0 - 9 * this.xyz_y * varU / ((varU - 4) * varV - varU * varV);
        this.xyz_z = (9 * this.xyz_y - 15 * varV * this.xyz_y - varV * this.xyz_x) / (3 * varV);
    }
    luvToLch() {
        this.lch_l = this.luv_l;
        this.lch_c = Math.sqrt(this.luv_u * this.luv_u + this.luv_v * this.luv_v);
        if (this.lch_c < 0.00000001) {
            this.lch_h = 0;
        }
        else {
            const hrad = Math.atan2(this.luv_v, this.luv_u);
            this.lch_h = hrad * 180.0 / Math.PI;
            if (this.lch_h < 0) {
                this.lch_h = 360 + this.lch_h;
            }
        }
    }
    lchToLuv() {
        const hrad = this.lch_h / 180.0 * Math.PI;
        this.luv_l = this.lch_l;
        this.luv_u = Math.cos(hrad) * this.lch_c;
        this.luv_v = Math.sin(hrad) * this.lch_c;
    }
    calculateBoundingLines(l) {
        const sub1 = Math.pow(l + 16, 3) / 1560896;
        const sub2 = sub1 > Hsluv.epsilon ? sub1 : l / Hsluv.kappa;
        const s1r = sub2 * (284517 * Hsluv.m_r0 - 94839 * Hsluv.m_r2);
        const s2r = sub2 * (838422 * Hsluv.m_r2 + 769860 * Hsluv.m_r1 + 731718 * Hsluv.m_r0);
        const s3r = sub2 * (632260 * Hsluv.m_r2 - 126452 * Hsluv.m_r1);
        const s1g = sub2 * (284517 * Hsluv.m_g0 - 94839 * Hsluv.m_g2);
        const s2g = sub2 * (838422 * Hsluv.m_g2 + 769860 * Hsluv.m_g1 + 731718 * Hsluv.m_g0);
        const s3g = sub2 * (632260 * Hsluv.m_g2 - 126452 * Hsluv.m_g1);
        const s1b = sub2 * (284517 * Hsluv.m_b0 - 94839 * Hsluv.m_b2);
        const s2b = sub2 * (838422 * Hsluv.m_b2 + 769860 * Hsluv.m_b1 + 731718 * Hsluv.m_b0);
        const s3b = sub2 * (632260 * Hsluv.m_b2 - 126452 * Hsluv.m_b1);
        this.r0s = s1r / s3r;
        this.r0i = s2r * l / s3r;
        this.r1s = s1r / (s3r + 126452);
        this.r1i = (s2r - 769860) * l / (s3r + 126452);
        this.g0s = s1g / s3g;
        this.g0i = s2g * l / s3g;
        this.g1s = s1g / (s3g + 126452);
        this.g1i = (s2g - 769860) * l / (s3g + 126452);
        this.b0s = s1b / s3b;
        this.b0i = s2b * l / s3b;
        this.b1s = s1b / (s3b + 126452);
        this.b1i = (s2b - 769860) * l / (s3b + 126452);
    }
    calcMaxChromaHpluv() {
        const r0 = Hsluv.distanceFromOrigin(this.r0s, this.r0i);
        const r1 = Hsluv.distanceFromOrigin(this.r1s, this.r1i);
        const g0 = Hsluv.distanceFromOrigin(this.g0s, this.g0i);
        const g1 = Hsluv.distanceFromOrigin(this.g1s, this.g1i);
        const b0 = Hsluv.distanceFromOrigin(this.b0s, this.b0i);
        const b1 = Hsluv.distanceFromOrigin(this.b1s, this.b1i);
        return Hsluv.min6(r0, r1, g0, g1, b0, b1);
    }
    calcMaxChromaHsluv(h) {
        const hueRad = h / 360 * Math.PI * 2;
        const r0 = Hsluv.distanceFromOriginAngle(this.r0s, this.r0i, hueRad);
        const r1 = Hsluv.distanceFromOriginAngle(this.r1s, this.r1i, hueRad);
        const g0 = Hsluv.distanceFromOriginAngle(this.g0s, this.g0i, hueRad);
        const g1 = Hsluv.distanceFromOriginAngle(this.g1s, this.g1i, hueRad);
        const b0 = Hsluv.distanceFromOriginAngle(this.b0s, this.b0i, hueRad);
        const b1 = Hsluv.distanceFromOriginAngle(this.b1s, this.b1i, hueRad);
        return Hsluv.min6(r0, r1, g0, g1, b0, b1);
    }
    hsluvToLch() {
        if (this.hsluv_l > 99.9999999) {
            this.lch_l = 100;
            this.lch_c = 0;
        }
        else if (this.hsluv_l < 0.00000001) {
            this.lch_l = 0;
            this.lch_c = 0;
        }
        else {
            this.lch_l = this.hsluv_l;
            this.calculateBoundingLines(this.hsluv_l);
            const max = this.calcMaxChromaHsluv(this.hsluv_h);
            this.lch_c = max / 100 * this.hsluv_s;
        }
        this.lch_h = this.hsluv_h;
    }
    lchToHsluv() {
        if (this.lch_l > 99.9999999) {
            this.hsluv_s = 0;
            this.hsluv_l = 100;
        }
        else if (this.lch_l < 0.00000001) {
            this.hsluv_s = 0;
            this.hsluv_l = 0;
        }
        else {
            this.calculateBoundingLines(this.lch_l);
            const max = this.calcMaxChromaHsluv(this.lch_h);
            this.hsluv_s = this.lch_c / max * 100;
            this.hsluv_l = this.lch_l;
        }
        this.hsluv_h = this.lch_h;
    }
    hpluvToLch() {
        if (this.hpluv_l > 99.9999999) {
            this.lch_l = 100;
            this.lch_c = 0;
        }
        else if (this.hpluv_l < 0.00000001) {
            this.lch_l = 0;
            this.lch_c = 0;
        }
        else {
            this.lch_l = this.hpluv_l;
            this.calculateBoundingLines(this.hpluv_l);
            const max = this.calcMaxChromaHpluv();
            this.lch_c = max / 100 * this.hpluv_p;
        }
        this.lch_h = this.hpluv_h;
    }
    lchToHpluv() {
        if (this.lch_l > 99.9999999) {
            this.hpluv_p = 0;
            this.hpluv_l = 100;
        }
        else if (this.lch_l < 0.00000001) {
            this.hpluv_p = 0;
            this.hpluv_l = 0;
        }
        else {
            this.calculateBoundingLines(this.lch_l);
            const max = this.calcMaxChromaHpluv();
            this.hpluv_p = this.lch_c / max * 100;
            this.hpluv_l = this.lch_l;
        }
        this.hpluv_h = this.lch_h;
    }
    hsluvToRgb() {
        this.hsluvToLch();
        this.lchToLuv();
        this.luvToXyz();
        this.xyzToRgb();
    }
    hpluvToRgb() {
        this.hpluvToLch();
        this.lchToLuv();
        this.luvToXyz();
        this.xyzToRgb();
    }
    hsluvToHex() {
        this.hsluvToRgb();
        this.rgbToHex();
    }
    hpluvToHex() {
        this.hpluvToRgb();
        this.rgbToHex();
    }
    rgbToHsluv() {
        this.rgbToXyz();
        this.xyzToLuv();
        this.luvToLch();
        this.lchToHpluv();
        this.lchToHsluv();
    }
    rgbToHpluv() {
        this.rgbToXyz();
        this.xyzToLuv();
        this.luvToLch();
        this.lchToHpluv();
        this.lchToHpluv();
    }
    hexToHsluv() {
        this.hexToRgb();
        this.rgbToHsluv();
    }
    hexToHpluv() {
        this.hexToRgb();
        this.rgbToHpluv();
    }
}
exports.Hsluv = Hsluv;
Hsluv.hexChars = "0123456789abcdef";
Hsluv.refY = 1.0;
Hsluv.refU = 0.19783000664283;
Hsluv.refV = 0.46831999493879;
Hsluv.kappa = 903.2962962;
Hsluv.epsilon = 0.0088564516;
Hsluv.m_r0 = 3.240969941904521;
Hsluv.m_r1 = -1.537383177570093;
Hsluv.m_r2 = -0.498610760293;
Hsluv.m_g0 = -0.96924363628087;
Hsluv.m_g1 = 1.87596750150772;
Hsluv.m_g2 = 0.041555057407175;
Hsluv.m_b0 = 0.055630079696993;
Hsluv.m_b1 = -0.20397695888897;
Hsluv.m_b2 = 1.056971514242878;
