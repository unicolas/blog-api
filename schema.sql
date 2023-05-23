--
-- PostgreSQL database dump
--

-- Dumped from database version 14.5 (Debian 14.5-2.pgdg110+2)
-- Dumped by pg_dump version 15.2

-- Started on 2023-05-07 17:30:15

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- TOC entry 4 (class 2615 OID 2200)
-- Name: public; Type: SCHEMA; Schema: -; Owner: -
--

-- *not* creating schema, since initdb creates it


SET default_table_access_method = heap;

--
-- TOC entry 209 (class 1259 OID 16385)
-- Name: comments; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.comments (
    id uuid NOT NULL,
    title text NOT NULL,
    content text NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    post_id uuid NOT NULL,
    user_id uuid NOT NULL,
    parent_comment_id uuid
);


--
-- TOC entry 210 (class 1259 OID 16390)
-- Name: posts; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.posts (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    title text NOT NULL,
    content text NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    user_id uuid NOT NULL
);


--
-- TOC entry 213 (class 1259 OID 16450)
-- Name: tags; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.tags (
    post_id uuid NOT NULL,
    term text NOT NULL
);


--
-- TOC entry 211 (class 1259 OID 16396)
-- Name: user_credentials; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.user_credentials (
    user_id uuid NOT NULL,
    password text NOT NULL
);


--
-- TOC entry 212 (class 1259 OID 16401)
-- Name: users; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.users (
    id uuid NOT NULL,
    username text NOT NULL,
    email text NOT NULL
);


--
-- TOC entry 3184 (class 2606 OID 16407)
-- Name: comments comments_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.comments
    ADD CONSTRAINT comments_pkey PRIMARY KEY (id);


--
-- TOC entry 3189 (class 2606 OID 16409)
-- Name: posts posts_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posts
    ADD CONSTRAINT posts_pkey PRIMARY KEY (id);


--
-- TOC entry 3198 (class 2606 OID 16463)
-- Name: tags tags_pk; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tags
    ADD CONSTRAINT tags_pk PRIMARY KEY (post_id, term);


--
-- TOC entry 3192 (class 2606 OID 16411)
-- Name: user_credentials user_credentials_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_credentials
    ADD CONSTRAINT user_credentials_pkey PRIMARY KEY (user_id);


--
-- TOC entry 3194 (class 2606 OID 16413)
-- Name: users username_u; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT username_u UNIQUE (username);


--
-- TOC entry 3196 (class 2606 OID 16415)
-- Name: users users_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


--
-- TOC entry 3190 (class 1259 OID 16416)
-- Name: fki_fk_user_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX fki_fk_user_id ON public.user_credentials USING btree (user_id);


--
-- TOC entry 3185 (class 1259 OID 16444)
-- Name: fki_parent_comment_id_fk; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX fki_parent_comment_id_fk ON public.comments USING btree (parent_comment_id);


--
-- TOC entry 3186 (class 1259 OID 16417)
-- Name: fki_post_id_fk; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX fki_post_id_fk ON public.comments USING btree (post_id);


--
-- TOC entry 3187 (class 1259 OID 16418)
-- Name: fki_user_id_fk; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX fki_user_id_fk ON public.comments USING btree (user_id);


--
-- TOC entry 3204 (class 2606 OID 16457)
-- Name: tags fk_post_id; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tags
    ADD CONSTRAINT fk_post_id FOREIGN KEY (post_id) REFERENCES public.posts(id) ON DELETE CASCADE;


--
-- TOC entry 3203 (class 2606 OID 16419)
-- Name: user_credentials fk_user_id; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_credentials
    ADD CONSTRAINT fk_user_id FOREIGN KEY (user_id) REFERENCES public.users(id) NOT VALID;


--
-- TOC entry 3199 (class 2606 OID 16445)
-- Name: comments parent_comment_id_fk; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.comments
    ADD CONSTRAINT parent_comment_id_fk FOREIGN KEY (parent_comment_id) REFERENCES public.comments(id) ON DELETE CASCADE NOT VALID;


--
-- TOC entry 3200 (class 2606 OID 16424)
-- Name: comments post_id_fk; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.comments
    ADD CONSTRAINT post_id_fk FOREIGN KEY (post_id) REFERENCES public.posts(id) ON DELETE CASCADE;


--
-- TOC entry 3201 (class 2606 OID 16429)
-- Name: comments user_id_fk; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.comments
    ADD CONSTRAINT user_id_fk FOREIGN KEY (user_id) REFERENCES public.users(id) NOT VALID;


--
-- TOC entry 3202 (class 2606 OID 16434)
-- Name: posts user_id_fk; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posts
    ADD CONSTRAINT user_id_fk FOREIGN KEY (user_id) REFERENCES public.users(id) NOT VALID;


-- Completed on 2023-05-07 17:30:15

--
-- PostgreSQL database dump complete
--

